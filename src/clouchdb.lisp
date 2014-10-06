;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLOUCHDB; Base: 10 -*-

;;; Copyright (c) 2007 Peter Eddy. All rights reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :clouchdb)

(defvar *default-host* "localhost" "CouchDb server host name")
(defvar *default-port* "5984" "The IANA assigned CouchDb port")
(defvar *default-db-name* "default" "Default database name")
(defvar *default-protocol* "http" "http or https")
(defvar *default-content-type* "application/octet-stream")
(defvar *view-function-names* '(map reduce validate-doc-update))
(defvar *debug-requests* nil)
(defvar *temp-db-counter* 0 "")
(defvar *use-pool* nil)

(defstruct (db (:constructor new-db))
  host port name protocol user password
  document-fetch-fn document-update-fn
  connection-pool)

(defclass http-connection ()
  ((stream :type stream
           :initform nil
           :accessor http-connection-stream
           :documentation "If non-nil, this object holds an open connection to the http server."))
  (:documentation "An instance of a pooled connection to the CouchDB database."))

(defun close-http-connection (connection)
  (let ((stream (http-connection-stream connection)))
    (when stream
      (handler-case
          (close stream)
        (error (condition) (warn "Error when closing pooled connection: ~s" condition))))))

(defun make-http-connection-pool (&key (capacity 20))
  (pooler:make-pool :name "CouchDB http connection pool"
                    :capacity capacity
                    :item-maker #'(lambda () (make-instance 'http-connection))
                    :item-destroyer #'close-http-connection))

(defun make-default-db ()
  (new-db :host *default-host*
          :port *default-port*
          :name *default-db-name*
          :protocol *default-protocol*
          :connection-pool (make-http-connection-pool)))

(defvar *couchdb* (make-default-db) "A db struct object")

(defvar *text-types* 
  '(("text" . nil) 
    (nil . "json") 
    (nil . "xml"))
  "Defined to instruct Drakma to treat json responses as text")

(defmacro define-constant (name value &optional doc)
  "A version of DEFCONSTANT for /strict/ CL implementations."
  ;; See <http://www.sbcl.org/manual/Defining-Constants.html>
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +utf-8+ (flexi-streams:make-external-format :utf-8 :eol-style :lf)
  "Default external format for document content.")

(defun document-update-notify (fn doc)
  "Optionally invoke specified function with supplied document, used
  to invoke user-specified hook functions."
  (if fn (funcall fn doc) doc))

;;
;; URL Parameter helpers
;;

(defun true-if-true (value)
  "Return \"true\" if value is non-nil, otherwise nil"
  (when value "true"))

(defun false-if-false (value)
  "Return \"false\" if value is nil, otherwise nil"
  (unless value "false"))

(defparameter *changes-options*
  '((:feed . ((:name . "feed") (:fn . keyword-to-http-param)))
    (:since . ((:name . "since") (:fn . value-as-string)))
    (:style . ((:name . "style") (:fn . keyword-to-http-param)))
    (:heartbeat . ((:name . "heartbeat") (:fn . value-as-string)))
    (:timeout . ((:name . "timeout") (:fn . value-as-string)))
    (:include-docs . ((:name . "include_docs") (:fn . true-if-true)))
    (:filter . ((:name . "filter") (:fn . identity))))
  "Parameters for the changes function.")

(defparameter *view-options*
  `((:key . ((:name . "key") (:fn . document-to-json)))
    (:start-key . ((:name . "startkey") (:fn . document-to-json)))
    (:start-key-docid . ((:name . "startkey_docid") (:fn . document-to-json)))
    (:end-key . ((:name . "endkey") (:fn . document-to-json)))
    (:end-key-docid . ((:name . "endkey_docsid") (:fn . document-to-json)))
    (:limit . ((:name . "limit") (:fn . value-as-string)))
    (:stale . ((:name . "stale") (:fn . value-as-string)))
    (:descending . ((:name . "descending") (:fn . true-if-true)))
    (:skip . ((:name . "skip") (:fn . document-to-json)))
    (:group . ((:name . "group") (:fn . true-if-true)))
    (:group-level . ((:name . "group_level") (:fn . document-to-json)))
    (:reduce . ((:name . "reduce") (:fn . false-if-false)))
    (:include-docs . ((:name . "include_docs") (:fn . true-if-true)))
    (:update . ((:name . "update") (:fn . false-if-false))))
  "Definitions for how invoke-view keyword parameters are translated
  into CouchDb parameters")

(defun transform-param (param value table)
  "Use a keyword transformation table to traslate between function
  keyword parameter names and values, and URL parameter names and
  values."
  (let ((transf (cdr (assoc param table))))
    (when transf
      (let ((value (funcall (cdr (assoc :fn transf)) value)))
        (when value
          (cons (cdr (assoc :name transf)) value))))))

(defun transform-params (keyword-params options)
  "Transform each keyword parameter using the specified set of
options, use only those transformations that return a non-nil result."
  (loop for param on keyword-params by #'cddr
     when (transform-param (first param) (second param) options)
     collect it))

;;
;; Conditions
;;

(define-condition clouchdb-error (error)
  ()
  (:documentation "The base type of all errors signaled by clouchdb"))

(define-condition authorization-error (clouchdb-error)
  ((result :initarg :result :reader result)
   (db :initarg :db :reader db)
   (uri :initarg :uri :reader uri)
   (text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream "Authorizion failed for URI \"~A\", reason \"~A\"" 
                     (uri condition)
                     (text condition)))))

(define-condition db-existential-error (clouchdb-error)
  ((text :initarg :uri :reader uri)
   (db :initarg :db :reader db)
   (result :initarg :result :reader result)))

(define-condition db-does-not-exist (db-existential-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "Database \"~A\" at \"~A\" does not exist" 
		     (db-name (db condition))
		     (uri condition)))))

(define-condition db-already-exists (db-existential-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "Database \"~A\" at \"~A\" already exists" 
		     (db-name (db condition))
		     (uri condition)))))

(define-condition illegal-database-name (db-existential-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "Illegal database name \"~A\" at \"~A\"" 
		     (db-name (db condition))
		     (uri condition)))))

(define-condition doc-error (clouchdb-error) 
  ((text :initarg :uri :reader text)
   (reason :initarg :reason :reader reason)
   (id :initarg :id :reader id))
  (:report (lambda (condition stream)
	     (format stream "Reason \"~A\", Document ID: \"~A\""
		     (reason condition)
		     (id condition)))))
  
(define-condition invalid-design-doc (doc-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Reason \"~A\", URI: \"~A\""
                     (reason condition)
                     (text condition)))))

(define-condition id-or-revision-conflict (doc-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "ID or Revsion Conflict. ID=\"~A\", Reason=~S"
		     (id condition) (reason condition)))))

(define-condition id-missing (doc-error)
  ()
  (:report (lambda (condition stream) 
             (declare (ignore condition))
             (format stream "No ID specified"))))

(define-condition invalid-id (doc-error)
  ((id-value :initarg :id-value :reader id-value))
  (:report (lambda (condition stream) 
             (format stream "Invalid ID: ~a" (id-value condition)))))

(define-condition invalid-document (doc-error)
  ((value :initarg :value :reader value))
  (:report (lambda (condition stream)
             (format stream "Value ~s is not a Document"
                     (value condition)))))

(define-condition document-missing (doc-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "No document found for ID=\"~A\""
		     (id condition))))
  (:documentation "Error raised when no document matching ID is found"))

(define-condition attachment-missing (doc-error)
  ((attachment-name :initarg :attachment-name :reader att-name)
   (attachments :initarg :attachments :reader attachments))
  (:report (lambda (condition stream)
             (format stream "No attachment named \"~A\" found for
             document ID \"~A\", known attachments: ~s"
                     (att-name condition)
                     (id condition)
                     (attachments condition))))
  (:documentation "Error raised when specified attachment is not found"))

(define-condition ps-view-def-error (clouchdb-error)
  ((ps-view-def :initarg :ps-view-def :reader ps-view-def))
  (:report (lambda (condition stream)
             (format stream "Invalid ps-view definition: ~s"
                     (ps-view-def condition))))
  (:documentation "Error raised for invalid ps-view definition"))

(define-condition invalid-type (clouchdb-error)
  ((input :initarg :input :reader input)
   (description :initarg :description :reader description))
  (:report (lambda (condition stream)
	     (format stream "Invalid input \"~A\", Description=~S"
		     (input condition) (description condition)))))

;;
;; Unexported utility functions
;;

(defun value-as-string (value)
  (cond ((numberp value)
	 (write-to-string value))
	((stringp value)
	 value)))

(defun value-as-integer (value)
  (cond ((numberp value)
         value)
        ((stringp value)
         (parse-integer value))))

(defun string-join (list &key (delim ",") (ignore-nil nil))
  "Join list of strings into a single result. Strings are delimited by
specified delimiter. If ignore-nil is true, then nil strings in the
list are skipped, and no delimiter is output."
  (with-output-to-string (out)
    (loop for (element . more?) on list do
         (unless (and (null element) ignore-nil)
           (if element
               (write-string element out))
           (when more?
             (write-string delim out))))))

(defmacro cat (&rest rest)
  "Shorthand for (concatenate 'string)"
  `(concatenate 'string ,@rest))

(defun doublequote (value)
  "Wrap specified value in double quotes."
  (cat "\"" value "\""))

(defun convert-encoding (string encoding)
  "Convert string to specified encoding. This may be totally wrong and
probably way too inefficient, but it seems to work."
  (flexi-streams:octets-to-string 
   (flexi-streams:string-to-octets string :external-format encoding)))

(defmacro url-encode (string)
  "URL-encode a string. Use drakma's url-encode since it's exported now"
  `(drakma:url-encode ,string +utf-8+))

(defun couchdb-host-url (db)
  (cat (db-protocol db) "://" (db-host db) ":" (db-port db)))

(defun couchdb-database-url (db)
  (cat (couchdb-host-url db) "/" (db-name db)))

(defun make-db-identifier (input)
  "Make a database identifier from either a string or db structure."
  (cond ((stringp input) input)
        ((db-p input) (couchdb-database-url input))
        (t (error 'invalid-type
                  :input input
                  :description 
                  "Database must be a string or a database structure"))))

(defun make-uri (&rest rest)
  "Return a URI containing protocol://host:port/ and the concatenation
of the remaining parameters."
  (concatenate 'string 
               (couchdb-host-url *couchdb*) "/"
	       (apply #'concatenate 'string rest)))

(defun json-stream-or-string-to-document (stream-or-string)
  "Read a json document from eiter a stream or a string. If the source
is a stream close the stream before returning. Return a document."
  (cond ((streamp stream-or-string)
         (with-open-stream (in stream-or-string)
           (json-to-document (with-output-to-string (out)
                               (loop for line = (read-line in nil)
                                  while (and line (write-line line out)))))))
        ((stringp stream-or-string)
         (json-to-document stream-or-string))))

(defmacro ensure-db (&body body)
  "Wrap request in code to check for errors due to non-existant data
bases. This is necessary because in a document operation, CouchDb does
not distinguish between an error due to a missing document and a
missing database."
  (let ((result (gensym)))
    `(let ((,result (progn ,@body)))
       (when (and (listp ,result) 
                  (equal "not_found" (document-property :|error| ,result))
                  (equal "no_db_file" (document-property :|reason|, result)))
         (error 'db-does-not-exist
                :result ,result :db *couchdb* :uri (make-uri)))
       ,result)))

(defun document-as-hash (doc)
  "Convert a document to a hashtable if it isn't one already. Document
  should be in the form of an associative list."
  (cond ((not (hash-table-p doc))
	 (let ((new-doc (make-hash-table)))
	   (dolist (e doc)
	     (setf (gethash (car e) new-doc) (cdr e)))
	   new-doc))
	(t doc)))

(defun doc-as-alist (doc)
  "Convert a document in the form of a hash table into an associative
  list"
  (cond ((hash-table-p doc)
	 (let ((new-doc))
	   (maphash #'(lambda (k v) (push (cons k v)  new-doc)) doc)
	   new-doc))
	(t doc)))

(defun as-keyword-symbol (value)
  "Return value in a form that would be used to identify the car of a
value in a document. For example, a value"
  (cond ((stringp value)
         (intern value "KEYWORD"))
        ((keywordp value)
         value)
        ((symbolp value)
         (as-keyword-symbol (intern (symbol-name value) "KEYWORD")))
        (t value)))

(defun as-field-name-string (value)
  "Convert a case-encoded symbol to a potentially mixed case string."
  (cond ((symbolp value)
         (symbol-name value))
        (t value)))

(defun keyword-to-http-param (keyword-symbol)
  "Convert a keword symbol that may contain hyphen characters to a
lower case string with any hyphens replaced by underscores:
':all-the-best' -> 'all_the_best'."
  (substitute #\_ #\- 
              (string-downcase 
               (cond ((keywordp keyword-symbol)
                      (as-field-name-string keyword-symbol))
                     (t keyword-symbol)))))

;; (defun keyword-to-http-param (keyword-symbol)
;;   "Convert a keword symbol that may contain hyphen characters to a
;; lower case string with any hyphens replaced by underscores:
;; ':all-the-best' -> 'all_the_best'."
;;   (substitute #\_ #\- 
;;               (string-downcase 
;;                (cond ((keywordp keyword-symbol)
;;                       (as-field-name-string keyword-symbol))
;;                      ((stringp keyword-symbol)
;;                       keyword-symbol)
;;                      (t keyword-symbol)))))


(defun document-property (name doc)
  "Get the value associated with the document property or nil if there
is no associated value. Note that name may be either a keyword symbol,
a regular symbol or a string. The <b>name</b> parameter may be either
a single keyword identifier (document property identifier) or it may
be a list of identifiers."
  (cond ((or (null name) (null doc))
         doc)
        ((listp name)
         (if (> (length name) 1)
             (document-property (rest name)
                                (document-property (car name) doc))
             (document-property (car name) doc)))
        (t (let ((name (as-keyword-symbol name)))
             (cond ((hash-table-p doc)
                    (gethash name doc))
                   (t (cdr (assoc name doc))))))))

(defun (setf document-property) (value name doc)
  "Allows setting of existing document properties in
place (destructively). The name paramter may be either a single
keyword identifier (document property identifier) or it may be a list
of identifiers. If the specified document property does not already
exist it is created."
  (labels ((recursive-compose (pl val)
             (if (null pl)
                 val
                 (list (cons (car pl) 
                             (recursive-compose (cdr pl) val))))))
    (let ((kw-name (as-keyword-symbol name)))
      (cond ((and (listp name) (> (length name) 1))
             (cond ((null (document-property (car name) doc))
                    ;; Specified property does not exist in current
                    ;; document, fill in potentially nested value.
                    (setf doc (nconc doc (recursive-compose name value))))
                   ((not (assoclp (document-property (car name) doc)))
                    ;; Value being set is replacing existing value
                    ;; which is not a more deeply nested document
                    ;; value. 
                    (rplacd (assoc (as-keyword-symbol (car name)) doc)
                            (recursive-compose (rest name) value))
                    doc)
                   (t
                    (setf (document-property 
                           (rest name)
                           (document-property (car name) doc))
                          value)
                    doc)))
            ((listp name)
             (setf (document-property (car name) doc) value))
            ((hash-table-p doc)
             (setf (gethash kw-name doc) value))
            (t 
             (let ((v (assoc kw-name doc)))
               (if (or (null v) (not (listp v)))
                   (setf doc (nconc doc (list (cons kw-name value))))
                   (rplacd v value))
               doc))))))

(defun set-document-property (doc &rest args)
  "Set a property of a document. If the named property does not exist,
add it to the document, otherwise change the existing value.  Does not
destructively modify input document, so be sure to use return value."
  (let ((doc (copy-tree doc)))
    (loop for (name value) on args by #'cddr
       do (setf doc (setf (document-property name doc) value)))
    doc))

(defun document-id (doc)
  "Shortcut for getting the ID from the specified document. First
  checks for :|_id| property, then :|id|"
  (cond ((stringp doc)
         doc)
        ((or (null doc) (not (listp doc)))
         (error 'invalid-document :value doc))
        (t
         (let ((id (or (document-property :|_id| doc)
                       (document-property :|id| doc))))
           (unless id
             (error 'invalid-document :value doc))
           id))))

(defun document-revision (doc-or-id)
  "Return the revision number for the document, identified by either
the document ID, the actual document, or the result of an add or
update that returns the revision as :|rev|"
  (cond ((stringp doc-or-id)
         (document-revision (get-document doc-or-id)))
        (t (or (document-property :|_rev| doc-or-id)
               (document-property :|rev| doc-or-id)))))

(defun query-document (query doc)
  "Return a list of all values in the document matching the query. For
example, given the document:

  ((:values (((:a . 1) (:b . 2)) ((:a . 3) (:b . 4)))))

the query string '(:values :a) will return (3 1), i.e. the value of
both :a associations. 

One special query input value is :* which is a 'wildcard'. With the
document described above the query '(:values :*) will return (4 3 2
1), or the values of all associations directly below :values. The
query '(:* :*) on this document will also return (4 3 2 1).

Another special query input value is :**, which recursively matches
the next query input. For example, with the following document:

  ((:level1 . ((:level2 . (((:level3 . 1)))))))

The query '(:** :level3) will return (1), the value
of :level3. Finally, functions can specified in the query. Functions
are called with the portion of the document matched to the previous
query element and can either return the document, return a different
document or null."
  (let ((res))
    (labels ((q (query doc rec)
               ;;(format t "~%test: r=~s, query=~s doc=~s~%" rec query doc)
               (cond ((null doc)
                      nil)
                     ((null query)
                      (push doc res))
                     ((eq :** (car query))
;;                      (format t "action: :**~%")
                      (q (cdr query) doc t))
                     ((and (listp query) (eq :** (car query)))
;;                      (format t "action: (:**)~%")
                      (q (cdr query) doc t))
                     ((assoclp doc)
                      ;; (format t "action: assoclp doc=~s ~%" doc)
                      (dolist (e doc)
                        (q query e rec)))
                     ((functionp (car query))
                      ;; (format t "action: functionp~%")
                      (q (cdr query) (funcall (car query) doc) rec))
                     ((keyword-assocp doc) 
                      ;; (format t "action: keyword-assocp doc=~S~%" doc)
                       (cond ((or (eq (car query) (car doc)) (eq :* (car query)))
                              ;; (format t "action: keyword asscoc=t~%" doc)
                              (q (cdr query) (cdr doc) nil))
                             ((and rec (listp (cdr doc)))
                              (q query (cdr doc) t))))
                     ((listp doc)
                      ;; (format t "action: listp~%")
                      (dolist (e doc)
                        (q query e rec)))
                     (t nil))))
      (q query doc nil)
      res)))

;;
;;
;;

(defun db-request (uri &rest args &key &allow-other-keys)
  "Used by most Clouchdb APIs to make the actual REST request."
  (let* ((drakma:*text-content-types* *text-types*)
         (want-stream (getf args :want-stream))
         (connection (and *use-pool*
                          (not want-stream)
                          (pooler:fetch-from (db-connection-pool *couchdb*)))))
    (multiple-value-bind (body status headers ouri stream must-close reason-phrase)
        (apply #'drakma:http-request (make-uri uri)
               `(,@args :basic-authorization
                        ,(when (db-user *couchdb*)
                               (list (db-user *couchdb*)
                                     (db-password *couchdb*)))
                        ,@(when connection
                                (list :stream (http-connection-stream connection)
                                      :close nil))))
      (declare (ignore ouri))
      (unwind-protect
           (progn
             (when *debug-requests*
               (format t "uri: ~s~%args: ~s~%must-close:~s~%reason-phrase: ~s~%
status: ~s~%headers: ~s~%stream:~s~%body:~s~%" 
                       uri args must-close reason-phrase status headers stream body))
             (if (stringp body) 
                 (values (json-to-document body) status)
                 (values body status reason-phrase)))
        (unless want-stream
          (if must-close
              ;; If the connection must be closed, then we can simply drop the
              ;; pooled connection.
              (close stream)
              ;; ELSE preserve the cached connection and return the object to the pool
              (when connection
                (setf (http-connection-stream connection) stream)
                (pooler:return-to (db-connection-pool *couchdb*) connection))))))))

(defun make-db (&key host port name protocol 
                  (user nil user-supplied-p) 
                  (password nil password-supplied-p)
                  document-fetch-fn document-update-fn 
                  (db *couchdb*)
                  (pool-capacity 20))
  "Create, populate and return a database structure from the current
special variables and any supplied keyword parameters, the latter take
precedence over the special variables."
  (new-db :host (or host (db-host db) *default-host*)
          :port (or port (db-port db) *default-port*)
          :name (or name (db-name db) *default-db-name*)
          :protocol (or protocol (db-protocol db) *default-protocol*)
          :user (if user-supplied-p user (db-user db))
          :password (if password-supplied-p password (db-password db))
          :document-fetch-fn (or document-fetch-fn (db-document-fetch-fn db))
          :document-update-fn (or document-update-fn (db-document-update-fn db))
          :connection-pool (make-http-connection-pool :capacity pool-capacity)))

(defun set-connection (&rest args &key host port name protocol user password 
                       document-fetch-fn document-update-fn db)
  "Set top-level connection information. The port may be specified as
a string or number. As of CouchDb version 7.2 the default port is
5984, prior to that it was 8888."
  (declare (ignore host port name protocol user password document-update-fn
                   document-fetch-fn db))
  (setf *couchdb* (apply #'make-db args)))

(defmacro with-connection ((&rest args &key (db *couchdb*)
                                  name port protocol host user password
                                  document-update-fn document-fetch-fn)
                           &body body)
  "Execute body in the context of the specified database connection
information.."
  (declare (ignore host port name protocol user password document-update-fn
                   document-fetch-fn db))
  `(let ((*couchdb* (make-db ,@args)))
     (progn ,@body)))

(defun document-properties (document)
  "Return the document properties, filtering out any couchdb reserved
properties (properties that start with an underscore)."
  (remove-if #'(lambda (e) (equal "_" (subseq (symbol-name (car e)) 0 1))) 
             document))

(defun couchdb-document-properties (document)
  "Return only CouchDb specific document properties (opposite of
document-properties)."
  (remove-if-not #'(lambda (e) (equal "_" (subseq (symbol-name (car e)) 0 1))) 
                 document))

(defmacro db-or-db-name (db)
  ""
  `(cond ((stringp ,db)
          (make-db :name ,db))
         ((db-p ,db) ,db)
         (t nil)))

(defun make-db-auth (db)
  "Return user name password values or nil if no user name specified
for db"
  (let ((user (db-user db)))
    (if user (list user (db-password db)))))

;;
;; CouchDB Database Management API
;;

(defun list-dbs (&optional (db *couchdb*))
  "Return a list of all databases managed by the current CouchDb
host."
  (let ((*couchdb* db))
    (db-request "_all_dbs" :method :get)))

(defun get-stats (&optional (db *couchdb*))
  "Get database statistics overview."
  (let ((*couchdb* db))
    (db-request "_stats" :method :get)))

(defun get-active-tasks (&optional (db *couchdb*))
  "Get active tasks for database or nil."
  (let ((*couchdb* db))
    (db-request "_active_tasks" :method :get)))

(defun create-db (&key (db *couchdb*) (if-exists :fail))
  "Create database. The db parameter may be either a string which is
the name of the database to create or an instance of a db
structure. If db is unspecified, uses *couchdb*. If database already
exists an error condition is raised. This condition can be avoided by
specifying :ingore for if-exists. In this case no error condition is
generated. Specify :recreate to potentially delete and create a new
database."
  (let ((*couchdb* (db-or-db-name db)))
    (multiple-value-bind (res status)
        (db-request (cat (url-encode (db-name *couchdb*)) "/")
                    :method :put :content "")
      (cond ((eq 201 status)
             res)
            ((equal "unauthorized" (document-property :|error| res))
             (error 'authorization-error
                    :text (document-property :|reason| res)
                    :result res
                    :db *couchdb*
                    :uri (make-uri (db-name *couchdb*))))
            ((equal "file_exists" (document-property :|error| res))
             (ecase if-exists
               ((:ignore) (list (cons :|ok| t) (cons :|ignored| t)))
               ((:recreate) (delete-db) (create-db))
               ((:fail)
                (restart-case
                    (error 'db-already-exists
                           :result res 
                           :db *couchdb*
                           :uri (make-uri (db-name *couchdb*)))
                  (ignore () :report "Ignore error and continue" nil)))))
            ((or (equal "illegal_database_name" (document-property :|reason| res))
                 (equal "illegal_database_name" (document-property :|error| res)))
             (error 'illegal-database-name
                    :result res
                    :db *couchdb*
                    :uri (make-uri (db-name *couchdb*))))))))

(defun delete-db (&key (db *couchdb*) if-missing)
  "Delete database. If db and db-name are unspecified, deletes
database named in *couchdb*. Normally deletion of non-existent databases
generates an error condition, but this can be avoided by
specifying :ignore in the if-missing parameter."
  (let* ((*couchdb* (db-or-db-name db)))
    (multiple-value-bind (res status)
        (db-request (cat (url-encode (db-name *couchdb*)) "/")
                    :method :delete
                    :basic-authorization (make-db-auth *couchdb*))
      (cond ((eq 200 status)
             res)
            ((equal "unauthorized" (document-property :|error| res))
             (error 'authorization-error
                    :text (document-property :|reason| res)
                    :result res
                    :db *couchdb*
                    :uri (make-uri (db-name *couchdb*))))
            ((and (document-property :|error| res) (not (eq :ignore if-missing)))
             (restart-case 
                    (error 'db-does-not-exist
                           :result res :db *couchdb* :uri (make-uri))
                  (ignore () :report "Ignore error and continue" nil))))
      res)))

(defun compact-db (&key (db *couchdb*))
  "Start compaction on current database, or specified database if
supplied. The db parameter, if supplied, is either a local database
name string or a db struct."
  (let ((*couchdb* (db-or-db-name db)))
    (ensure-db ()
      (db-request (cat (db-name *couchdb*) "/_compact") 
                  :method :post
                  :content-type "application/json"
                  :content ""))))

(defun get-couchdb-info (&key (db *couchdb*))
  "Get information from the couchdb server."
  (let ((*couchdb* db))
    (db-request nil :method :get)))

(defun get-db-info (&key (db *couchdb*))
  "Get information for named database, return ((:|error|
  . \"not_found\") (:|reason| . \"no_db_file\")) if database does not
  exist. The db parameter, if supplied, is either a local database
  name string or a db struct."
  (let ((*couchdb* (db-or-db-name db)))
    (db-request (cat (url-encode (db-name *couchdb*)) "/")
                :method :get)))

(defun create-temp-db-name ()
  "Return a database name that's probably unique."  
  (concatenate 'string 
	       "temp-" 
	       (write-to-string (get-universal-time)) "-"
	       (write-to-string (incf *temp-db-counter*))))

(defun create-temp-db (&key (db-name-creator #'create-temp-db-name))
  "Create a temporary database."
  (let ((db (make-db :name (funcall db-name-creator))))
    (let ((res (create-db :db db)))
      (if (document-property :|error| res)
	  (error (format t "Error ~S creating database: ~A"
		       (document-property :|error| res) (db-name db)))))
    db))

(defmacro with-temp-db (&body body)
  "Execute body in context of newly created, temporary
database. Delete database before return."
  (let ((result (gensym)))
    `(let* ((*couchdb* (create-temp-db))
            (,result))
       (unwind-protect
            (setf ,result (progn ,@body))
         (delete-db))
       ,result)))

(defun changes (&rest options &key (db *couchdb*) feed since style
                heartbeat timeout filter notify-fn include-docs)
  "Get document change activity from current database or database
specified in db parameter. The :feed keyword parameter value indicates
how to poll for changes. Valid values for this parameter
include :longpoll to block waiting for a single change
response, :continuous to poll for changes indefinately, or :normal to
not poll (the default) and instead return a document containing
changes. If specified, the :style keyword parameter may be either
:main-only (the default) or :all-docs for more revision information.

If specified, the notify-fn will be called as each change notification
is recieved from the server. The notify-fn should return nil to signal
that it no longer wishes to receive change notificaitons. At that
point the stream will be closed and the changes function will return.

If :longpoll or :continuous is specified as the feed parameter but no
notify-fn is provided, this function will return the feed stream. It
is the caller's responsibility to close the stream."
  (declare (ignore since style timeout heartbeat include-docs))
  (let ((*couchdb* (db-or-db-name db))
        (want-stream (find feed '(:continuous :longpoll))))
    (ensure-db ()
         (multiple-value-bind (res status)
             (db-request (cat (url-encode (db-name *couchdb*)) "/_changes")
                         :parameters (transform-params options *changes-options*)
                         :want-stream want-stream :method :get)
           (cond ((not (equal status 200))
                  (let ((doc (json-stream-or-string-to-document res)))
                    (cond ((equal "invalid design doc" (document-property :|reason| doc))
                           (error 'invalid-design-doc
                                  :id filter :uri (make-uri (db-name *couchdb*) "/" filter)
                                  :reason (document-property :|reason| doc))))))
                 ((and notify-fn (streamp res))
                  (with-open-stream (in res)
                    (loop for line = (read-line res nil :eof)
                       while (and line
                                  (not (equal line :eof))
                                  (funcall notify-fn (json-to-document line))))))
                 (t res))))))

(defun replicate (target &key (source *couchdb*) (create-target nil))
  "Replicate current database to target, or source to target if source
is specified. Source and target database values must either be strings
or database structures. Use strings to specify simple local database
names, use database structures to specify either local or remote
databases. If true, create-target will cause the replication target to
be created automatically, as of CouchDb version 0.11."
    (ensure-db ()
      (db-request "_replicate"
                  :method :post
                  :basic-authorization (make-db-auth *couchdb*)
                  :content-type "application/json"
                  :content 
                  (cat "{\"source\":\"" (make-db-identifier source) "\","
                       "\"target\":\"" (make-db-identifier target) "\","
                       (if create-target "\"create_target\":true") "}"))))

;;
;; Users and Authentication
;;

(defun add-admin (name password &key (db *couchdb*))
  "Add an admin user."
  (let ((*couchdb* db))
    (db-request (cat "_config/admins/" name)
                :method :put
                :content (doublequote password))))

(defun get-session (&optional (db *couchdb*))
  "Get cookie based login user information."
  (let ((*couchdb* db))
    (db-request (cat "_session")
                :method :get)))

(defun set-session (&optional (db *couchdb*))
  "Do cookie based login."
  (let ((*couchdb* db))
    (db-request (cat "_session")
                :method :post)))

(defun logout-session (&optional (db *couchdb*))
  "Logout cookie based session."
  (let ((*couchdb* db))
    (db-request (cat "_session")
                :method :delete)))

(defun get-config (&key (db *couchdb*) section (option nil option-provided-p))
  "Get database configuration."
  (let ((*couchdb* db))
    (multiple-value-bind (res status)
        (db-request (cat "_config" 
                         (if section (cat "/" (as-field-name-string section))))
                    :method :get
                    :basic-authorization (make-db-auth *couchdb*))
      (cond ((eq 200 status)
             (if option-provided-p (document-property option res) res))
            ((equal "unauthorized" (document-property :|error| res))
             (error 'authorization-error
                    :text (document-property :|reason| res)
                    :result res
                    :db *couchdb*
                    :uri (make-uri (db-name *couchdb*))))
            (t res)))))

;;
;; CouchDB Document Management API
;;

(defun get-uuids (&key (count 1) (db *couchdb*))
  "Returns one or more new UUID from the current database."
  (let ((*couchdb* db))
    (values (db-request "_uuids" 
                        :parameters 
                        (list (cons "count" (value-as-string count)))
                        :method :get))))

(defun get-all-documents (&rest options &key key keys start-key
                        start-key-docid end-key end-key-docid limit
                        stale descending skip group group-level reduce
                        include-docs)
  "Get a listing of all documents in a database. This method
implements the same keyword parameters as the view API."
  (declare (ignore key start-key start-key-docid end-key end-key-docid
                   limit stale descending skip group group-level
                   reduce include-docs))
  (ensure-db ()
    (db-request (cat (url-encode (db-name *couchdb*)) "/_all_docs")
		:method (if keys :post :get)
                :content-type "application/json"
		:parameters (transform-params options *view-options*)
                :content (if keys (document-to-json `((:|keys| . ,keys)))))))

(defun get-document (id &key revision revisions conflicts
                     revision-info (if-missing nil if-missing-p))
  "Get a document by ID. Returns nil if the document does not exist.
The revision property specifies an optional revision number, if
unspecified, the most recent revision is returned. The revisions and
revision-info parameters, if non-nil, request revision information
about the document instead of the actual document contents. The
revision-info option contains more revision information than
revisions. All revision* options are mutually exclusive, specify only
one."
  (unless id
    (error 'id-missing))
  (let ((parameters)
        (doc-id (document-id id)))
    (when conflicts (push (cons "conflicts" "true") parameters))
    (when revision
      (push (cons "rev" (value-as-string revision)) parameters))
    (when revisions
      (push (cons "revs" "true") parameters))
    (when revision-info
      (push (cons "revs_info" "true") parameters))
    (let ((res (ensure-db () (db-request (cat (url-encode 
                                               (db-name *couchdb*)) 
                                              "/" 
                                              (url-encode doc-id))
					 :method :get 
					 :parameters parameters))))
      (if (document-property :|error| res)
          (progn
            (cond ((eq if-missing :ignore)
                   nil)
                  ((and if-missing-p (not (eq if-missing :error)))
                   (if (functionp if-missing)
                       (funcall if-missing doc-id)
                       if-missing))
                  (t (error 'document-missing :id doc-id))))
	  (document-update-notify 
           (db-document-fetch-fn *couchdb*) res)))))
		      
(defun encode-file (file)
  "Encode a file in the format suitable for CouchDb attachments"
  (with-output-to-string (out)
    (with-open-file (in file)
      (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (with-open-file (stream file :element-type '(unsigned-byte 8))
          (read-sequence data stream)
          (s-base64:encode-base64-bytes data out nil))))))

(defun encode-attachments (attachments)
  "Encode the list of attachements, return them in an _attachments
document fragment."
  (let ((encoded))
    (when attachments
      (dolist (a attachments)
        (format t "file name: ~S~%" (car a))
        (let ((e (encode-file (car a))))
          (when e
            (push `(,(as-keyword-symbol (second a)) . 
                     ((:|type| . "base64")
                      (:|data| . ,e)))
                  encoded))))
      `(:|_attachments| . ,encoded))))

(defun put-document (doc &key id attachments)
  "Create a new document or update and existing one. If the document
is new an ID must be specified (but see post-document). If the
document has been fetched from the server (and still has its :_id
property) then no ID need be specified. If an ID is specified and it
differs from the existing :_id value, then a new document is created
with the new ID and the non-special properties of the specified
document, since the latter would generate a CouchDb error."
  (let ((current-id (document-property :|_id| doc)))
    (cond ((not (or current-id id))
	   (error 'id-missing))
	  ;; If an ID was specified and that ID does not match the
	  ;; :_id property of the current document, strip the document
	  ;; of all special (CouchDb managed) properties, since these
	  ;; are specific to the current document. The presence of the
	  ;; ID parameter in this situation means 'create a new
	  ;; document with the same contents as the old one'.
	  ((and id current-id (not (equal current-id id)))
	   (setf doc (document-properties doc))))
    (when attachments
      (setf doc (cons (encode-attachments attachments) doc)))
    (let ((res (ensure-db () 
                 (db-request (cat (url-encode (db-name *couchdb*)) "/" 
                                  (url-encode (if id id current-id)))
                             :content-type "application/json"
                             :external-format-out +utf-8+
                             :content-length nil
                             :content (document-to-json 
                                       (document-update-notify 
                                        (db-document-update-fn *couchdb*) 
                                        doc))
                             :method :put))))
      (when (document-property :|error| res)
        (error (if (equal "conflict" (document-property :|error| res)) 
                   'id-or-revision-conflict 
                   'doc-error)
               :id (if id id current-id)
               :reason (document-property :|reason| res)))
      res)))

(defun post-document (doc)
  "Post the document to the server, creating a new document. An
existing _id in the document will be ignored, the server will create a
new document and assign a new ID. Therefore this is an easy method for
copying documents. The return value includes the document ID in
the :ID property."
  (let ((res (ensure-db ()
               (db-request (cat (url-encode (db-name *couchdb*)) "/")
                           :content-type "application/json"
                           :external-format-out +utf-8+
                           :content-length nil
                           :method :post
                           :content (document-to-json 
                                     (document-update-notify 
                                      (db-document-update-fn *couchdb*) 
                                      doc))))))
    (when (document-property :|error| res)
      (error 'doc-error :id nil :reason (document-property :|reason| res)))
    res))

(defun create-document (doc &key id attachments)
  "Create a new document, optionally specifying the new document
ID."
  (if id
      (put-document doc :id id :attachments attachments)
      (post-document doc)))

(defun copy-document (source destination &key revision)
  "Copy source document to destination. The source parameter may be
  either a document ID or a document from which the ID will be
  obtained. The destination parameter may also be a document ID or
  document. If the destination document does not already exist it will
  be created. 

  If the destination document does exist and the intention is to
  overwrite that document, then the destination document revision must
  be specified. If the destination parameter is a document then the
  revision information will be taken from that document unless
  the :revision parameter is specified. The revision parameter must be
  the current revision of the destination document. Alternatively the
  revision parameter may be the keyword
  :current which will cause this function to fetch the current
  revision number from the database."
  (let ((rev (cond ((eq :current revision)
                    (document-revision (get-document destination)))
                   ((and (not revision) (listp destination))
                    (document-revision destination))
                   (t revision)))
        (dest-id (document-id destination)))
    (ensure-db ()
      (let ((res (db-request (cat (url-encode (db-name *couchdb*)) "/" 
                                  (url-encode (document-id source)))
                             :content-type "text/plain"
                             :external-format-out +utf-8+
                             :content-length nil
                             :method :copy
                             :additional-headers 
                             `(("Destination" . 
                                              ,(if rev
                                                   (cat dest-id "?rev=" rev) 
                                                   dest-id))))))
        (when (document-property :|error| res)
          (error (if (equal "conflict" (document-property :|error| res))
                     'id-or-revision-conflict 
                     'doc-error)
                 :id dest-id
                 :reason (document-property :|reason| res)))
        res))))
                
(defun all-docs-by-seq (&rest options &key key keys start-key
                        start-key-docid end-key end-key-docid limit
                        stale descending skip group group-level reduce
                        include-docs)
  "Fetch a list of all documents that were updated and deleted, in the
order these actions are done."
  (declare (ignore key start-key start-key-docid end-key end-key-docid
                   limit stale descending skip group group-level
                   reduce include-docs))
  (ensure-db ()
    (db-request (cat (url-encode (db-name *couchdb*)) "/_all_docs_by_seq")
                :method (if keys :post :get)
                :content-type "application/json"
                :parameters (transform-params options *view-options*)
                :content (if keys (document-to-json `((:|keys| . ,keys)))))))

(defun as-deleted-document (doc)
  "Return specified document in a format used by bulk-document-update
to indicate that the document should be deleted in the bulk
operation."
  (set-document-property (couchdb-document-properties doc)
                         :|_deleted| t))

(defun bulk-document-update (docs &key all-or-nothing)
  "Update multiple documents in a single request. The <b>docs</b>
  parameter is a list of documents. Any document in the list that does
  not contain an :|_id| value is created with a CouchDb assigned
  ID. Documents that contain a '(:|_deleted| . t) top-level property
  will be deleted. Documents that contain an :|_id| property will be
  updated. If all-or-nothing is true then all operations must succeed
  for any to succeed, default is false."
  (ensure-db () 
    (db-request (cat (url-encode (db-name *couchdb*)) "/_bulk_docs")
		:method :post
                :content-type "application/json"
                :external-format-out +utf-8+
                :content-length nil
		:content 
                (cat "{ "
                     (if all-or-nothing "\"all_or_nothing\": true,")
                     "\"docs\": [ " 
		     (string-join 
                      (mapcar #'document-to-json docs))
		     " ]}"))))

(defun delete-document (doc-or-id &key revision (if-missing :error))
  "Delete a document. The doc-or-id parameter may be either the
 document ID or the document itself. If the doc-or-id value is the
 document ID and no revision parameter is specified, then the document
 is fetched from the server to get the current revision, since the
 revision is necessary to complete the operation. If the doc-or-id
 value is a document then the revision value in the document is used
 unless a different revision is specified. At most one revision of the
 document will be deleted."
  (labels ((del (id rev)
             (let ((res (ensure-db () 
                          (db-request 
                           (cat (url-encode (db-name *couchdb*)) "/" 
                                (url-encode id)
                                "?rev=" 
                                (url-encode (value-as-string rev)))
                           ;; Authorization here is for delete-view,
                           ;; it's not required for normal document
                           ;; deletes
                           :basic-authorization (make-db-auth *couchdb*)
                           :method :delete))))
               (when (document-property :|error| res)
                 (error 'doc-error :id id 
                        :reason (document-property :|reason| res)))
               res)))
    (cond ((null doc-or-id)
           (error 'id-missing))
          ((stringp doc-or-id)
           (let ((rev (or revision
                          (document-revision 
                           (get-document doc-or-id
                                         :if-missing
                                         (if (eq :error if-missing)
                                             :error 
                                             nil))))))
             (if rev
                 (del doc-or-id rev)
                 (if (eq :ignore if-missing) nil if-missing))))
          ((listp doc-or-id)
           (delete-document (document-id doc-or-id)
                             :revision (or revision (document-revision doc-or-id))
                             :if-missing if-missing)))))

;;
;; Attachment API
;;
;; Support for what CouchDb calls "Standalone Attachments".
;;

(defun attachment-name (attachment)
  "Return the name of an attachment, possibly converting from the
keyword symbol of the clouchdb document to a string e.g. :|text.txt|
to 'text.txt', or the car of a single document attachment list element
e.g, '(:|text.txt| (:|stub| . T) (..)) to 'text.txt'."
  (cond ((symbolp attachment)
         (symbol-name attachment))
        ((and attachment (listp attachment))
         (attachment-name (car attachment)))
        (t attachment)))

(defun attachment-list (doc-or-id &key fetch)
  "List attachments associated with document. If the document id is
specified in the first parameter then this function will fetch the
corresponding document from the server in order to get the attachment
list, otherwise it will simply return the list of attachments in the
specified document unless keyword parameter fetch is true."
  (document-property :|_attachments| 
                     (cond ((stringp doc-or-id)
                            (get-document doc-or-id))
                           ((listp doc-or-id)
                            (if fetch 
                                (get-document (document-id doc-or-id))
                                doc-or-id))
                           (t (error 'invalid-document :value doc-or-id)))))

(defun add-attachment (doc-or-id content &key name revision
                       (content-type *default-content-type*))
  "Attach content to a document identified by either the id or doc
parameter. If the document does not already exist it will be created."
  (let* ((id (cond ((stringp doc-or-id) doc-or-id)
                   ((and (not (null doc-or-id)) (listp doc-or-id))
                    (document-id doc-or-id))
                   ((null doc-or-id) (error 'id-missing))
                   (t (error 'invalid-id :id-value doc-or-id))))
         (rev (or revision 
                  (document-revision 
                   (if (not (stringp doc-or-id))
                       doc-or-id
                       (get-document id :if-missing :ignore)))))
         (name (or name
                   (if (pathnamep content) (file-namestring content)))))
    (ensure-db ()
      (let ((url (cat (url-encode (db-name *couchdb*)) "/"
                      (url-encode id) "/" (url-encode name)
                      (if rev (cat "?rev=" rev)))))
        (db-request url :method :put
                    :content-type content-type
                    :content content)))))

(defun delete-attachment (doc-or-id attachment &key revision)
  "Delete (detach) an attachment. The attachment parameter is either
the name of the file to remove from the document, which can be either
a string or a keyword symbol (as obtained from the :|_attachments|
value of a document) or it's one element of the list of attachments in
a document."
  (let* ((id (if (stringp doc-or-id)
                 doc-or-id 
                 (document-id doc-or-id)))
         (rev (or revision 
                  (document-revision 
                   (if (not (stringp doc-or-id))
                       doc-or-id
                       (get-document id)))))
         (name (attachment-name attachment)))
    (ensure-db ()
      (db-request (cat (url-encode (db-name *couchdb*)) "/"
                       (url-encode id) "/" 
                       (url-encode (if (symbolp name) 
                                       (symbol-name name)
                                       name))
                       (if rev (cat "?rev=" rev)))
                  :method :delete))))

(defun get-attachment-stream (doc-or-id attachment 
                              &key (force-binary t))
  "Get specified attachment as a stream. The caller is responsible for
closing the stream. Return stream and HTTP status"
  (let ((name (attachment-name attachment))
        (id (if (stringp doc-or-id) doc-or-id (document-id doc-or-id))))
    (ensure-db ()
      (multiple-value-bind (res status)
          (db-request (cat (url-encode (db-name *couchdb*)) "/"
                           (url-encode id) "/"
                           (url-encode name))
                      :force-binary force-binary
                      :want-stream t)
        (cond ((not (eql status 200))
               (let ((doc (get-document id)))
                 (when doc (error 'attachment-missing
                                  :id id 
                                  :attachment-name name
                                  :attachments 
                                  (attachment-list doc)))))
              ((streamp res) res)
              (t (values res)))))))

(defun save-attachment (doc-or-id attachment path &key
                        (if-does-not-exist :create)
                        (if-exists :supersede))
  "Save specified attachement from specified document to path on file
system. The doc-or-id parameter must either be a document ID string or
the actual document. The attachment parameter is either the string
value of the attachment name, e.g. \"file.jpg\", a keyword symbol as
returned in the car of the list of attachments, .e.g. :|file.jsp|, or
one of the elements of a document's attachment list,
e.g: (:|file.jsp| (:|stub| . T) (:|content_type|
. \"image/jpeg\") (:|length| . 3543434)).

If the path identifies a directory then the target file will be
created in that directory with the same name as the attachment in the
document. If the path ends with a file name the attachment will be
created with that name."
  (let ((in (get-attachment-stream doc-or-id attachment))
        (output-path (if (> (length (file-namestring path)) 0)
                         path
                         (merge-pathnames (pathname path)
                                          (pathname 
                                           (attachment-name attachment))))))
    (unwind-protect
         (cond ((streamp in)
                (with-open-file (output output-path 
                                        :direction :output 
                                        :element-type 'octet
                                        :if-does-not-exist if-does-not-exist
                                        :if-exists if-exists)
                  (let ((out (flexi-streams:make-flexi-stream output)))
                    (loop for line = (read-line in nil nil)
                       while line
                       do (write-line line out))))
                output-path)
               (t nil))
      (if in (close in)))))

(defmacro with-attachment ((stream doc-or-id attachment) &body body)
  "Passed doc-or-id and attachment parameters to get-attachment-stream
to open the input stream and ensures that the stream is automatically
closed after execution of the statements in the body."
  `(let ((,stream (get-attachment-stream ,doc-or-id ,attachment)))
     (unwind-protect
          (progn ,@body)
       (if ,stream (close ,stream)))))

;;
;; Design Document Functions

(defun ensure-design-doc (id)
  "Return specified design document, creating it if it does not already exist."
  (get-document (cat "_design/" id) 
                :if-missing #'(lambda (id) 
                                (create-document nil :id id))))

;;
;; Views API
;;

(defun ad-hoc-view (view &rest options &key key start-key
                    start-key-docid end-key end-key-docid limit stale
                    descending skip group group-level reduce
                    include-docs (language "javascript"))
  "Execute query using an ad-hoc view."
  (declare (ignore key start-key start-key-docid end-key end-key-docid
                   limit stale descending skip group group-level
                   reduce include-docs))
  (ensure-db ()
    (db-request (cat (url-encode (db-name *couchdb*)) "/_temp_view")
		:method :post
                :external-format-out +utf-8+
		:content-type "application/json"
                :content-length nil
                :parameters (transform-params options *view-options*)
		:content 
                (cat "{\"language\" : \"" language "\"," 
                     "\"map\" : \"" (remove #\Newline view)
                     "\"}"))))

(defun create-view (id view &key (language "javascript"))
  "Create one or more views in the specified view document ID."
  (let ((res (ensure-db ()
                        (db-request (cat (url-encode (db-name *couchdb*)) 
                                         "/_design/" 
                                         (url-encode id))
                                    :method :put
                                    :external-format-out +utf-8+
                                    :basic-authorization (make-db-auth *couchdb*)
                                    :content-type "application/json"
                                    :content-length nil
                                    :content
                                    (cat "{\"language\" : \"" language "\"," 
                                         "\"views\" : {"
                                         (remove #\Newline view) "}}")))))
    (when (document-property :|error| res)
      (error (if (equal "conflict" (document-property :|error| res))
                 'id-or-revision-conflict
                 'doc-error)
             :id id :reason (document-property :|reason| res)))
    res))

(defun create-ps-view (id &rest view-defs)
  "Create one or more views in the specified view document ID."
  (create-view id (string-join view-defs)))

(defun validate-ps-view (defun fn-name fn-param fn-body)
  "Validation for ps-view definition"
  (declare (ignore fn-body))
  (cond ((not (eq 'defun defun))
         (error 'ps-view-def-error :ps-view-def 
                "View definition should take the form (defun <function> (params) (....)"))
        ((not (find fn-name *view-function-names*))
         (error 'ps-view-def-error :ps-view-def
                (format nil "Valid function names are ~{~s ~}" *view-function-names*)))
        ((and (eq fn-name 'map) (not (eq 1 (length fn-param))))
         (error 'ps-view-def-error :ps-view-def
                "map takes one parameter, e.g.: (defun map (doc) (... (emit ...))"))
        ((and (eq fn-name 'reduce) (not (eq 2 (length fn-param))))
         (error 'ps-view-def-error :ps-view-def
                "reduce takes two parameters, e.g.: (defun reduce (keys values) (...))"))))

(defmacro ps-function (&body body)
  "Create a view using parenscript"
  `(with-output-to-string (out)
     (write-string "{" out)
     (write-string 
      (string-join
       (list 
         ,@(mapcar #'(lambda (fn)
                       (destructuring-bind (defun fn-name fn-param fn-body) fn
                         (declare (ignore defun))
                         `(cat "\"" 
                               (string-downcase (symbol-name (quote ,fn-name)))
                               "\": \""
                               (parenscript::ps (lambda (,@fn-param) ,fn-body))
                               "\"")))
                   body))
       :ignore-nil t) out)
     (write-string "}" out)))

(defmacro ps-view ((&optional view-name) &body body)
  "Create a view using parenscript"
  `(with-output-to-string (out)
     (when ,view-name
       (write-string (cat "\"" ,view-name "\":") out))
     (write-string "{" out)
     (write-string 
      (string-join
       (list 
         ,@(mapcar #'(lambda (fn)
                       (destructuring-bind (defun fn-name fn-param fn-body) fn
                         (validate-ps-view defun fn-name fn-param fn-body)
                         `(cat "\"" 
                               (string-downcase (symbol-name (quote ,fn-name)))
                               "\": \""
                               (parenscript::ps (lambda (,@fn-param) ,fn-body))
                               "\"")))
                   body))
       :ignore-nil t) out)
     (write-string "}" out)))

(defmacro defpsfun (&body body)
  "Define a parenscript function in the object format used by
CouchDb. Specifically, a property name associated with a
lambda. E.g.:  {'foo' : function () {...}}"
  `(ps-function (defun ,@body)))

;; (add-ps-lists 
;;  "foo" 
;;  (defpsfun foo (d r) (return (* 7 d.type)))
;;  (defpsfun bar (d r) (return d.size)))

(defun delete-view (id &key revision if-missing)
  "Delete identified view document"
  (ensure-db ()
    (delete-document (cat "_design/" (url-encode id))
                     :revision revision :if-missing if-missing)))

(defun invoke-view (id view &rest options &key key start-key
                    start-key-docid end-key end-key-docid limit stale
                    descending skip group group-level reduce
                    include-docs)
  "Invoke a view by specifiying the document ID that contains the view
and the name of the contained view. The key parameter specifies an
optional value to match against the view's mapped field. The start-key
and end-key values specify the optional begin and end range of the
mapped field(s) of each document to return. If descending is t,
returns results in reverse order. If update is t, does not refresh
view for query, use for higher performance but possible data
inconsistency."
  (declare (ignore key start-key start-key-docid end-key end-key-docid
                   limit stale descending skip group group-level
                   reduce include-docs))
  (ensure-db ()
    (db-request (cat (url-encode (db-name *couchdb*)) "/_design/" 
                     (url-encode id) "/_view/" (url-encode view))
		:method :get
                :parameters (transform-params options *view-options*))))

(defun view-util (cmd)
  "General function called by view functions"
  (multiple-value-bind (res status)
      (db-request (cat (db-name *couchdb*) cmd)
                  :method :post)
    (cond ((eq 202 status)
           res)
          ((document-property :|error| res)
           (error 'doc-error
                  :id cmd
                  :text (document-property :|error| res)
                  :reason (document-property :|reason| res))))))

(defun view-cleanup ()
  "Clean up old view data"
  (view-util "/_view_cleanup"))

(defun compact-view (view-name)
  "Compact named view"
  (view-util (cat "/_compact/" view-name)))

(defun add-functions (design-doc-id type &rest list-defs)
  "Add lists in list-defs to design document identified by
design-doc-id. If the document does not exist, create it. If any list
function definitions already exist in the document, update them."
  (let* ((doc-id (cat "_design/" design-doc-id))
         (doc (get-document doc-id :if-missing :ignore)))
    (dolist (list-def (mapcar #'json-to-document list-defs))
      (format t "list-def: ~a~%" list-def)
      (setf doc (setf (document-property (list type (caar list-def)) doc)
                      (cdar list-def))))
    (put-document doc :id doc-id)))

(defun add-lists-fns (id &rest defs)
  "Add CouchDb lists in list-defs to document identified by id. If the
document does not exist, create it. If any list function definitions
already exist in the document, update them."
  (apply #'add-functions id :|lists| defs))

(defun invoke-list (doc-id list-id)
  ""
  (multiple-value-bind (body status headers uri stream must-close reason-phrase)
      (let ((url (make-uri (url-encode (db-name *couchdb*))
                                     "/_design/"
                                     (url-encode doc-id) 
                                     "/_list/"
                                     (url-encode list-id))))
        (format t "uri: ~S~%" url)
        (drakma:http-request url))
    (declare (ignore status headers uri must-close reason-phrase))
    (when stream
      (close stream))
    body))

;;
;; Shows
;;

(defun add-shows-fns (id &rest defs)
  "Add CouchDb lists in list-defs to document identified by id. If the
document does not exist, create it. If any list function definitions
already exist in the document, update them."
  (apply #'add-functions id :|shows| defs))


;(defmacro def-show-fn (name &body body)
;  (add-shows-fns

;(add-shows-fns 
; "foo")
;(defpsfun post (doc req)

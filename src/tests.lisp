;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLOUCHDB-EXAMPLES; Base: 10 -*-
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

(cl:in-package :cl-user)

(defpackage :clouchdb-tests
  (:use :cl :parenscript :lift :clouchdb))

(in-package :clouchdb-tests)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defparameter *people* 
  (list '((:name . "peter")
	  (:city . "boston")
	  (:friends . ("marc" "marie" "charles")))
	'((:name . "marc")
	  (:city . "paris")
	  (:friends . ("peter" "marie" "claire")))
	'((:name . "jean")
	  (:city . "new york")
	  (:friends . ("peter" "marie" "bob")))
	'((:name . "laurie")
	  (:city . "montpelier")
	  (:friends . ("peter" "marc" "marie")))
	'((:name . "michelle")
	  (:city . "paris")
	  (:friends . ("marie" "claire")))
	'((:name . "richard")
	  (:city . "san francisco")
	  (:friends . ("jean" "marie" "marc")))))

(defparameter *document0*
  '((:|total_rows| . 2) 
    (:|offset| . 0) 
    (:|rows| 
     ((:|id| . "id1") 
      (:|key| . "key1") 
      (:|value| 
        (:|_id| . "id1a") 
        (:INTEGER . 0) 
        (:NAME . "name1") 
        (:LIST . ("one" "two" "nine"))
        (:ACL 
         (:READ "reader1" "reader2") 
         (:WRITE "writer1" "writer2") 
         (:DELETE "deleter1") 
         (:GRANT "granter1") 
         (:REVOKE "revoker1"))))
     ((:|id| . "id2") 
      (:|key| . "key2") 
      (:|value| 
        (:|_id| . "id2a") 
        (:INTEGER . 1) 
        (:LIST . (a b c))
        (:NAME . "name2")
        (:ACL 
         (:READ "reader1" "reader3") 
         (:WRITE "writer1" "writer3") 
         (:DELETE "deleter1") 
         (:GRANT "granter2") 
         (:REVOKE "revoker1")))))))

;;
;; Test helper functions
;;

(defun create-test-documents (data &key (id-field nil))
  "Add specified data to database, if specified, the id-field key
identifies the associate key who's value should be used for the
document ID. If not specified, database will generate a unique key."
  (let ((created))
    (dolist (e data)
      (push (create-document e :id (if id-field (cdr (assoc id-field e))))
	    created))
    created))

(defun ensure-ids (data id-field)
  "Ensure there's a document for each element in the data."
  (let ((docs))
    (dolist (e data)
      (let ((doc (get-document (document-property id-field e))))
	(if (not (document-property :|_id| doc))
	    (error (format t "Document ID=~S not found" 
			   (document-property id-field e))))
	(push e docs)))
    (= (length data) (length docs))))

(defun contains-property (data pname &key pval (test #'equal))
  "Return data that contains named property. If property value is
  specified, match the value of the named property with pval."
  (let ((results))
    (dolist (d data)
      (let ((value (document-property pname d)))
	(cond ((and value (null pval))
	       (push d results))
	      ((listp value)
	       (when (find pval value :test test)
		 (push d results)))
	      (t (if (funcall test value pval)
		     (push d results))))))
    results))

;;
;; Database API Tests
;;
;; Tests ClouchDb APIs that create and delete databases and get
;; CouchDb server information.
;;

;;
;; General tests that do not require a db connection
;;

(deftestsuite clouchdb-general-tests () ())

(addtest (clouchdb-general-tests)
  (:documentation "Ensure document-property gets correct value from document")
  general-tests-document-property
  (ensure
   (let ((doc '((:NAME . "Value1") (:|Name| . "Value2") (:|NaMe| . "Value3"))))
     (reduce #'(lambda (a b) (and a b))
             (mapcar #'(lambda (e)
                         (equal (cdr (assoc (car e) doc))
                                (document-property (car e) doc)))
                     doc)))))


(addtest (clouchdb-general-tests)
  (:documentation "Ensure connection information is correctly carried over")
  clouchdb-with-connection0
  (with-connection (:name "wc-name" :port "3434" :protocol "https" :user "wc-user" 
                          :password "wc-pass" :document-fetch-fn #'clouchdb::delete-db
                          :document-update-fn #'clouchdb::create-db)
    (ensure-same (db-name *couchdb*) "wc-name")
    (ensure-same (db-port *couchdb*) "3434")
    (ensure-same (clouchdb::db-protocol *couchdb*) "https")
    (ensure-same (db-user *couchdb*) "wc-user")
    (ensure-same (db-password *couchdb*) "wc-pass")))

(addtest (clouchdb-general-tests)
  (:documentation "Ensure user can be set to nil")
  clouchdb-with-connection1
  (with-connection (:user "wc-user")
    (with-connection (:user nil)
      (ensure-same nil (db-user *couchdb*)))))

;;
;; (document-property) tests
;;


(addtest (clouchdb-general-tests)
  (:documentation "Test accessing document property with property list")
  general-tests-document-property-single
  (ensure-same 2
               (document-property ':one
                                  '((:one . 2) (:a . ((:b . ((:c . "found")))))))))

(addtest (clouchdb-general-tests)
  (:documentation "Test accessing document property with property list")
  general-tests-document-property-single-list
  (ensure-same 2
               (document-property '(:one)
                                  '((:one . 2) (:a . ((:b . ((:c . "found")))))))))

(addtest (clouchdb-general-tests)
  (:documentation "Test accessing document property with property list")
  general-tests-document-property-list-access
  (ensure-same "found"
               (document-property '(:a :b :c)
                                  '((:one . 2) (:a . ((:b . ((:c . "found")))))))))

;;
;; (setf document-property) tests
;;

(addtest (clouchdb-general-tests)
  (:documentation "Set existing document property with single property")
  general-tests-document-property-modify
  (let ((doc '((:one . 2) (:a . ((:b . ((:c . "not searched for"))))))))
    (ensure-same "found" 
                 (document-property :a 
                                    (setf (document-property :a doc) "found")))))

(addtest (clouchdb-general-tests)
  (:documentation "Set existing document property with single element list property")
  general-tests-document-property-single-list-modify
  (let ((doc '((:one . 2) (:a . ((:b . ((:c . "not searched for"))))))))
    (ensure-same "found" 
                 (document-property 
                  :a 
                  (setf (document-property '(:a) doc) "found")))))

(addtest (clouchdb-general-tests)
  (:documentation "Set existing document property with property list")
  general-tests-document-property-list-modify
  (let ((doc '((:one . 2) (:a . ((:b . ((:c . "not found")))))))
        (properties '(:a :b :c)))
    (ensure-same "found" 
                 (document-property 
                  properties
                  (setf (document-property properties doc) "found")))))

(addtest (clouchdb-general-tests)
  (:documentation "Add top level document property
  with (setf (document-property))")
  general-tests-document-property-add-top-level
  (let ((doc '((:one . 1) (:two . 2))))    
    (ensure-same 3 (document-property 
                    :three 
                    (setf (document-property :three doc) 3)))))

(addtest (clouchdb-general-tests)
  (:documentation "Add document property list to document.")
  general-tests-document-property-add-property-list
  (let ((doc '((:one . 1) (:two . 2)))
        (properties '(:three :four)))
    (ensure-same 4 
                 (document-property 
                  properties
                  (setf (document-property properties doc) 4)))))

(addtest (clouchdb-general-tests)
  (:documentation "Replace document property with nested property list.")
  general-tests-document-property-replace-property-list
  (let ((doc '((:one . 1) (:two . 2) (:three . 3)))
        (properties '(:three :four)))
    (ensure-same 4 
                 (document-property 
                  properties
                  (setf (document-property properties doc) 4)))))

(addtest (clouchdb-general-tests)
  (:documentation "Change nested document property.")
  general-tests-document-property-replace-property-list1
  (let ((doc '((:one . 1) (:two . ((:four . ((:nine . 8))))) (:three . 3)))
        (properties '(:two :four :nine)))
    (ensure-same 9
                 (document-property 
                  properties
                  (setf (document-property properties doc) 9)))))

(addtest (clouchdb-general-tests)
  (:documentation "Truncate deep document property list.")
  general-tests-document-property-replace-property-list2
  (let ((doc '((:one . 1) (:two . ((:four . ((:nine . 9))))) (:three . 3)))
        (properties '(:two :four)))
    (ensure-same 4
                 (document-property 
                  properties
                  (setf (document-property properties doc) 4)))))

(addtest (clouchdb-general-tests)
  (:documentation "Create document with single property (setf (document-property))")
  general-tests-document-property-create-property
  (ensure-same 44 
               (document-property 
                :value 
                (setf (document-property ':value nil) 44))))

(addtest (clouchdb-general-tests)
  (:documentation "Create document with property list (setf (document-property))")
  general-tests-document-property-create-propert-list1
  (ensure-same 44 
               (document-property 
                :value 
                (setf (document-property '(:value) nil) 44))))

(addtest (clouchdb-general-tests)
  (:documentation "Create document with property list (setf (document-property))")
  general-tests-document-property-create-propert-list-multi
  (ensure-same 44 
               (document-property 
                '(:some :nested :value)
                (setf (document-property '(:some :nested :value) nil) 44))))

;; 
;; 
;; 

(addtest (clouchdb-general-tests)
  (:documentation "Test case-encoded field name functions")
  general-tests-case-encoded
  (ensure-same "lowercase" (as-field-name-string (as-keyword-symbol "lowercase")))
  (ensure-same "MixedCase" (as-field-name-string (as-keyword-symbol "MixedCase")))
  (ensure-same "Mixed-Case-Hyphen" 
               (as-field-name-string (as-keyword-symbol "Mixed-Case-Hyphen")))
  (ensure-same "UPPER-CASE" 
               (as-field-name-string (as-keyword-symbol "UPPER-CASE"))))

(addtest (clouchdb-general-tests)
  (:documentation "test keyword-assocp for positive match")
  general-tests-keword-assocp-positivie
  (ensure (clouchdb::keyword-assocp '(:key . "value")))
  (ensure (clouchdb::keyword-assocp '(:key . 3)))
  (ensure (clouchdb::keyword-assocp '(:key . 'value)))
  (ensure (clouchdb::keyword-assocp '(:key . (1 2 3))))  
  (ensure (clouchdb::keyword-assocp '(:key . ((1 2 3)))))
  (ensure (clouchdb::keyword-assocp '(:key . ((:a . "aye") (:b . "bee"))))))

(addtest (clouchdb-general-tests)
  (:documentation "test keyword-assocp for positive match")
  general-tests-keword-assocp-negative
  (ensure-null (clouchdb::keyword-assocp '()))
  (ensure-null (clouchdb::keyword-assocp '(3 4)))
  (ensure-null (clouchdb::keyword-assocp '(abe lincolin))))

(addtest (clouchdb-general-tests)
  (:documentation "test assoclp function for positive match")
  general-tests-assoclp-positive
  (ensure (clouchdb::assoclp '((:a . b) (:c . "dee"))))
  (ensure (clouchdb::assoclp '((:a (1 2 3)))))
  (ensure (clouchdb::assoclp '((:a . nil) (:b . "froth")))))

(addtest (clouchdb-general-tests)
  (:documentation "test assoclp function for non-matches")
  general-tests-assoclp-negative
  (ensure-null (clouchdb::assoclp '()))
  (ensure-null (clouchdb::assoclp '(:a . 3)))
  (ensure-null (clouchdb::assoclp '(:a (1 2 3))))
  (ensure-null (clouchdb::assoclp '(:a (:b . "sea"))))
  (ensure-null (clouchdb::assoclp '(:a ((:b . "sea") (:d . "e"))))))
  ;;(ensure-null (clouchdb::assoclp '((:aye :bee :sea))))
  ;;(ensure-null (clouchdb::assoclp '((:aye :bee (:a . 3) (:b . "froth"))))))

(addtest (clouchdb-general-tests)
  (:documentation "*document0* query tests ")
  general-tests-document0-query
  (ensure-same 2 (car (query-document '(:|total_rows|) *document0*)))
  (ensure-same 2 (length (car (query-document '(:|rows|) *document0*))))
  (ensure-same 2 (length (query-document '(:|rows| :|value|) *document0*)))
  (ensure (progn
            (let ((res (query-document '(:|rows| :|value| :|_id|) *document0*)))
              (and (find "id2a" res :test #'equal)
                   (find "id1a" res :test #'equal)
                   (eql 2 (length res))))))
  (ensure-same 2 (length  (query-document '(:|rows| :|value| :acl) *document0*)))
  (ensure (progn
            (let ((res (query-document '(:|rows| :|value| :acl :read) *document0*)))
              (and (eql 2 (length res))
                   (find "reader1" (car res) :test #'equal)
                   (find "reader3" (car res) :test #'equal)
                   (find "reader1" (second res) :test #'equal)
                   (find "reader2" (second res) :test #'equal))))))
  
(addtest (clouchdb-general-tests)
  (:documentation "*document0* query wildcard tests ")
  general-tests-document0-query-wildcard-top
  (ensure-same 2 (car (query-document '(:|total_rows|) *document0*)))
  (ensure-same 2 (length (car (query-document '(:|rows|) *document0*))))
  (ensure-same 2 (length (query-document '(:|rows| :|value|) *document0*)))
  (ensure (progn
            (let ((res (query-document '(:** :|_id|) *document0*)))
              (and (find "id2a" res :test #'equal)
                   (find "id1a" res :test #'equal)
                   (eql 2 (length res))))))
  (ensure-same 2 (length  (query-document '(:** :acl) *document0*)))
  (ensure (progn
            (let ((res (query-document '(:** :read) *document0*)))
              (and (eql 2 (length res))
                   (find "reader1" (car res) :test #'equal)
                   (find "reader3" (car res) :test #'equal)
                   (find "reader1" (second res) :test #'equal)
                   (find "reader2" (second res) :test #'equal)))))
  (ensure (progn
            (let ((res (query-document '(:|rows| :** :read) *document0*)))
              (and (eql 2 (length res))
                   (find "reader1" (car res) :test #'equal)
                   (find "reader3" (car res) :test #'equal)
                   (find "reader1" (second res) :test #'equal)
                   (find "reader2" (second res) :test #'equal))))))

(addtest (clouchdb-general-tests)
  (:documentation "*document0* query wildcard tests ")
  general-tests-document0-query-wildcard-middle
  (ensure (progn
            (let ((res (query-document '(:|rows| :** :|_id|) *document0*)))
              (and (find "id2a" res :test #'equal)
                   (find "id1a" res :test #'equal)
                   (eql 2 (length res))))))
  (ensure-same 2 (length  (query-document '(:|rows| :** :acl) *document0*)))
  (ensure (progn
            (let ((res (query-document '(:|rows| :** :read) *document0*)))
              (and (eql 2 (length res))
                   (find "reader1" (car res) :test #'equal)
                   (find "reader3" (car res) :test #'equal)
                   (find "reader1" (second res) :test #'equal)
                   (find "reader2" (second res) :test #'equal))))))

(addtest (clouchdb-general-tests)
  (:documentation "*people* query tests")
  general-tests-people-query
  (ensure (progn
            (let ((res (query-document '(:name) *people*)))
              (and (eql 6 (length res))
                   (find "richard" res :test #'equal)
                   (find "michelle" res :test #'equal)
                   (find "laurie" res :test #'equal)
                   (find "jean" res :test #'equal)
                   (find "marc" res :test #'equal)
                   (find "peter" res :test #'equal))))))

;;
;; Db Administration Tests
;;
;; Test the APIs that create, delete, and get information about
;; databases or the server.
;;

(deftestsuite clouchdb-db-admin-tests () ())

(addtest (clouchdb-db-admin-tests)
  (:documentation "Look for the welcome message and version info from server")
  generic-server-info-query
  (ensure-same "Welcome" (document-property :|couchdb| (get-couchdb-info)))
  (ensure (document-property :|version| (get-couchdb-info))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Ensure get-db-info reports non-existant databases")
  db-non-existance-test
  (let ((*couchdb* (make-db :name (create-temp-db-name))))
    (ensure-same "not_found" (document-property :|error| 
                                                (get-db-info)))
    (ensure-same "no_db_file" (document-property :|reason|  (get-db-info)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Create a database and ensure it gets created")
  db-creation-test
  (with-temp-db
    (ensure-same (document-property :|db_name| (get-db-info))
                 (db-name *couchdb*))
    (ensure-same (document-property :|db_name| (get-db-info :db *couchdb*))
                 (db-name *couchdb*))
    (ensure-same 0 (document-property :|doc_count| (get-db-info :db *couchdb*)))
    (ensure-same 0 (document-property :|update_seq| (get-db-info :db *couchdb*)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Be sure differnt get-db-info function invocations return the same results")
  get-db-info-test
  (with-temp-db
    (ensure (tree-equal (get-db-info) (get-db-info :db *couchdb*) 
                        :test #'equal))
    (ensure (tree-equal (get-db-info) (get-db-info :db (db-name *couchdb*))
                        :test #'equal))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Make sure deleting a nonexistant db generates an error")
  db-delete-non-existant-db
  (ensure-condition 'db-does-not-exist (delete-db :db (create-temp-db-name))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Make sure deleting a nonexistant db error is ignoreable")
  db-ignore-delete-non-existant-db
  (ensure
   (document-property 
    :|error| 
    (delete-db :if-missing :ignore :db (create-temp-db-name)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Creating a db that already exists is an error")
  db-create-existant-db
  (ensure-condition 'db-already-exists 
    (with-temp-db 
      (create-db))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Creating a db that already exists is an error")
  db-create-existant-db-name
  (ensure-condition 'db-already-exists 
    (with-temp-db
      (db-name (create-db :db (db-name *couchdb*))))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Ignore the duplicate db create error")
  db-ignore-create-existant-db
  (ensure (document-property :|ok|
                             (with-temp-db
                               (create-db :if-exists :ignore)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "recreate option for create-db on existing db")
  db-recreate-db
  (ensure (document-property :|ok|
                             (with-temp-db
                               (create-db :if-exists :recreate)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "recreate option for create-db on non-existant db")
  db-recreate-nonexistant-db
  (with-temp-db
    (ensure (document-property :|ok| (create-db :if-exists :recreate)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "Test handling of illegal db name")
  db-create-illegal-db-name
  (ensure-condition 'illegal-database-name
    (db-name (create-db :db (make-db :name "FOO")))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "initate compaction")
  db-compact
  (with-temp-db
    (ensure (document-property :|ok| (compact-db)))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "initation compaction specifying db name")
  db-compact-name
  (with-temp-db
    (ensure (document-property :|ok| (compact-db :db (db-name *couchdb*))))))

(addtest (clouchdb-db-admin-tests)
  (:documentation "initation compaction specifying structure")
  db-compact-db-struct
  (with-temp-db
    (ensure (document-property :|ok| (compact-db :db *couchdb*)))))

;;
;; Document API Tests
;;

(deftestsuite clouchdb-doc-api-tests ()
  ()
  (:dynamic-variables
   (*couchdb* (make-db :db *couchdb*)))
  (:setup
   (set-connection :db (create-temp-db)))
  (:teardown 
   (delete-db)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Ensures the temporary db for these tests is succesfully created.")
  empty-test
  (ensure-same (document-property :|db_name| (get-db-info)) 
               (db-name *couchdb*)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Create a document with create-document")
  create-document-auto-id
  (ensure (document-property :|ok| (create-document '((:a . "test"))))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Create document with create-document, specify document ID")
  create-document-specified-id
  (ensure (document-property 
           :|ok| 
           (create-document '((:a . "test")) :id "specified"))))

(addtest (clouchdb-doc-api-tests)
  (:documentation 
   "Create a document with a duplicate ID and ensure revision conflict")
  create-document-specified-id-conflict
  (ensure (document-property :|ok| (create-document '((:a . "test")) 
                                                    :id "specified")))
  (ensure-condition 'id-or-revision-conflict
    (create-document '((:a "test")) :id "specified")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Create a document with put-document")
  put-document-create
  (ensure (document-property :|ok| (put-document '((:a "test")) :id "specified"))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Create a document with put-document with no ID (error)")
  put-document-create
  (ensure-condition 'id-missing (put-document '((:a "test")))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Get a non-existant document (error)")
  get-non-existant-document
  (ensure-condition 'document-missing (get-document "does-not-exist")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Get a non-existant document, ignore error")
  get-non-existant-document-ignore1
  (ensure-same nil
               (get-document "does-not-exist" :if-missing :ignore)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Get a non-existant document, use missing value")
  get-non-existant-document-missing-value
  (ensure-same "hi"
               (get-document "does-not-exist" :if-missing "hi")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test revision info")
  get-document-revision-info
  (ensure-same 11 (progn 
		    ;; Create a document with one field, f, and revize it a
		    ;; lot
		    (put-document '((:f . "empty")) :id "revizedalot")
		    (dotimes (i 10)
		      (let ((doc (get-document "revizedalot")))
			(setf (document-property :f doc) i)
			(put-document doc)))
		    ;;
		    (let ((docinf (get-document "revizedalot" :revision-info t)))
		      (length (document-property :|_revs_info| docinf))))))
	    
(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, creating destination with source ID")
  copy-document-create-dest-id
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (copy-document "source" "dest")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, creating destination with source document")
  copy-document-create-dest-doc
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (copy-document (get-document "source") "dest")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, overwrite destination by specifying revision")
  copy-document-overwrite-dest-1
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (create-document '((:dest . "data")) :id "dest")) 
  (ensure (copy-document "source" "dest" 
                         :revision (document-revision (get-document "dest")))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, overwrite destination by specifying current revision")
  copy-document-overwrite-dest-2
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (create-document '((:dest . "data")) :id "dest")) 
  (ensure (copy-document "source" "dest" :revision :current)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, overwrite destination by specifying dest document")
  copy-document-overwrite-dest-3
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (create-document '((:dest . "data")) :id "dest")) 
  (ensure (copy-document "source" (get-document "dest"))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, overwrite destination by specifying current revision")
  copy-document-overwrite-revision-conflict
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (create-document '((:dest . "data")) :id "dest")) 
  (ensure-condition 'id-or-revision-conflict (copy-document "source" "dest")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Copy document, overwrite destination by specifying current revision")
  copy-document-source-id-missing
  (ensure (create-document '((:source . "data")) :id "source"))
  (ensure (create-document '((:dest . "data")) :id "dest")) 
  (ensure-condition 'doc-error (copy-document "doesnotexist" "dest")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a document by ID")
  delete-document-by-id
  (ensure (document-property :|ok| (create-document '((:a "test")) :id "specified")))
  (ensure (document-property :|ok| (delete-document "specified"))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a document by ID and revision")
  delete-document-by-id-and-revision
  (ensure (progn
	    (create-document '((:a . "document")) :id "specified")
	    (let ((doc (get-document "specified")))
	      (document-property :|ok| 
                                 (delete-document (document-id doc)
                                                  :revision 
                                                  (document-revision doc)))))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a document by document")
  delete-document-by-document
  (ensure (progn
	    (create-document '((:a . "document")) :id "polly")
	    (document-property :|ok| 
			       (delete-document (get-document "polly"))))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a non-existant document")
  delete-document-bad-id
  (ensure-condition 'document-missing (delete-document "specified")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a non-existant document and ignore error")
  delete-document-missing-ignore
  (ensure-same nil (delete-document "specified" :if-missing :ignore)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Delete a non-existant document and return custom value")
  delete-document-missing-custom-value
  (ensure-same "hi" (delete-document "specified" :if-missing "hi")))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Add a bunch of documents and ensure they get created.")
  create-document-test1
  (ensure-same (length (create-test-documents *people* :id-field :name))
	       (length *people*))
  (ensure (ensure-ids *people* :name)))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test get-document for existing document.")
  get-document-test
  (ensure (document-property :|ok| (create-document '((:a . "test")) :id "test")))
  (ensure-same (document-property :|_id| (get-document "test")) "test"))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Update a document property")
  update-document-test1
  (ensure (create-test-documents *people* :id-field :name))
  (ensure (let ((d (get-document "peter")))
	    (setf (document-property :city d) "San Francisco")
	    (put-document d)
	    (equal "San Francisco"
		   (document-property :city (get-document "peter"))))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Add a property to an existing document")
  add-document-property-test1
  (ensure (create-test-documents *people* :id-field :name))
  (ensure 
   (document-property :|ok| (put-document (cons '(:handsome . "false")
					      (get-document "peter"))))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test bulk updates of documents")
  bulk-update-1
  (ensure (create-test-documents *people* :id-field :name))
  (ensure (let ((docs))
            (dolist (di (document-property :|rows| (get-all-documents)))
              (push (cons '(:new-field . "New Value") 
                          (get-document (document-property :|id| di)))
                    docs))
            (bulk-document-update docs)
	    (block test
	      (loop for di in (document-property :|rows| (get-all-documents)) do
		   (if (not (equal "New Value" 
				   (document-property :new-field
						      (get-document 
						       (document-property :|id| di)))))
		       (return-from test nil)))
	      t))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test document ID encoding")
  encode-document-id
  (ensure (document-property :|ok| 
                             (create-document '((:a "test")) 
                                              :id "http://google.com")))
  (ensure-same (document-id (get-document "http://google.com")) 
               "http://google.com"))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test UUID default function invocation")
  uuid-default-test
  (ensure-same 1 (length (document-property :|uuids| (get-uuids)))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test UUID default function invocation")
  uuid-count-test
  (ensure-same 3 (length (document-property :|uuids| (get-uuids :count 3)))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test encoding and decoding of utf-8 document IDs")
  encode-document-utf-8-ids
  (ensure 
   (let ((ids '("Ångström Café" "σπασμένα" "我能吞下玻璃而不伤身体")))
     (reduce #'(lambda (a b) (and a b))
             (mapcar #'(lambda (id)
                         (and (document-property :|ok| (create-document nil :id id))
                              (equal id (document-property :|_id| (get-document id)))))
                     ids)))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test document content encoding by creating a
  document with a field for a variety of languages, then fetching that
  document and comparing the fecthed data with the source data") 
  encode-document-contents-glass-eating
  (ensure
   (let ((glass-eaters '((:middle-english . "An preost wes on leoden, Laȝamon was ihoten")
                         (:classical-greek . "ὕαλον ϕαγεῖν δύναμαι· τοῦτο οὔ με βλάπτει.")
                         (:monotonic-greek . "Μπορώ να φάω σπασμένα γυαλιά χωρίς να πάθω τίποτα.")
                         (:polytonic-greek . "Μπορῶ νὰ φάω σπασμένα γυαλιὰ χωρὶς νὰ πάθω τίποτα.")
                         (:french . "Je peux manger du verre, ça ne me fait pas de mal.")
                         (:provençal . "Pòdi manjar de veire, me nafrariá pas.")
                         (:walloon . "Dji pou magnî do vêre, çoula m' freut nén må.")
                         (:spanish . "Puedo comer vidrio, no me hace daño.")
                         (:romanian . "Pot să mănânc sticlă și ea nu mă rănește.")
                         (:esperanto . "Mi povas manĝi vitron, ĝi ne damaĝas min.")
                         (:czech . "Mohu jíst sklo, neublíží mi.")
                         (:lithuanian . "Aš galiu valgyti stiklą ir jis manęs nežeidžia")
                         (:polska . "Mogę jeść szkło i mi nie szkodzi.")
                         (:macedonian . "Можам да јадам стакло, а не ме штета.")
                         (:russian . "Я могу есть стекло, оно мне не вредит.")
                         (:belarusian-cyrillic . "Я магу есці шкло, яно мне не шкодзіць.")
                         (:belarusian-lacinka . "Ja mahu jeści škło, jano mne ne škodzić.")
                         (:armenian . "Կրնամ ապակի ուտել և ինծի անհանգիստ չըներ։")
                         (:hebrew . "אני יכול לאכול זכוכית וזה לא מזיק לי")
                         (:yiddish . "איך קען עסן גלאָז און עס טוט מיר נישט װײ")
                         (:chinese . "我能吞下玻璃而不伤身体。")
                         (:chinese-traditional . "我能吞下玻璃而不傷身體。")
                         (:japanese . "私はガラスを食べられます。それは私を傷つけません。")
                         (:korean . "나는 유리를 먹을 수 있어요. 그래도 아프지 않아요")
                         (:euro-symbol . "€")
                         (:georgian . "მინას ვჭამ და არა მტკივა."))))
     (and (document-property :|ok| (create-document glass-eaters :id "glass-eaters"))
          (let ((doc (get-document "glass-eaters")))
            (reduce #'(lambda (a b) (and a b))
                    (mapcar #'(lambda (e) 
                                (equal (cdr e)
                                       (document-property (car e) doc)))
                            doc)))))))

;;
;; Attachments 
;;

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test adding attachment 0")
  add-attachment-0
  (ensure (document-property :|ok|
                             (add-attachment "doc" 
                                             (pathname "tests.lisp"))))
  (let ((attachments (attachment-list "doc")))
    (ensure-same 1 (length attachments))
    (ensure-same "tests.lisp" (attachment-name (car attachments)))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test adding attachment using name other than file name")
  add-attachment-1
  (ensure (document-property :|ok|
                             (add-attachment "doc" 
                                             (pathname "tests.lisp")
                                             :name "something.lisp")))
  (let ((attachments (attachment-list "doc")))
    (ensure-same 1 (length attachments))
    (ensure-same "something.lisp" (attachment-name (car attachments)))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test adding multiple attachments")
  add-attachment-2
  (ensure (document-property :|ok|
                             (add-attachment "doc" 
                                             (pathname "tests.lisp")
                                             :name "something.lisp")))
  (ensure (document-property :|ok|
                             (add-attachment "doc" 
                                             (pathname "tests.lisp")
                                             :name "something.else")))
  (let ((attachments (attachment-list "doc")))
    (ensure-same 2 (length attachments))
    (ensure (find :|something.lisp| attachments :key #'car))
    (ensure (find :|something.else| attachments :key #'car))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Test creating attachment with stream")
  add-attachment-stream
  (ensure (document-property :|ok|
                             (add-attachment "doc" 
                                             (pathname "tests.lisp"))))
  (ensure (document-property 
           :|ok| 
           (with-attachment (stream "doc" "tests.lisp")
             (add-attachment "doc2" stream :name "tests2.lisp"))))
  (let ((attachments (attachment-list "doc2")))
    (ensure-same 1 (length attachments))
    (ensure (find :|tests2.lisp| attachments :key #'car))))

(addtest (clouchdb-doc-api-tests)
  (:documentation "Make shure conflicts don't appear when they shouldn't")
  test-no-conflicts
  (ensure (create-document '((:name . "hi")) :id "hi"))
  (ensure-null (document-property :|_conflicts| (get-document "hi"))))

;; 
;; Replication Tests.
;;

(deftestsuite clouchdb-replication-tests () 
  ()
  (:dynamic-variables
   (*couchdb* (make-db)))
  (:setup
   (progn
     (set-connection :db (create-temp-db))
     (create-test-documents *people* :id-field :name)))
  (:teardown 
   (progn 
     (delete-db)
     (set-connection :name "default"))))

(addtest (clouchdb-replication-tests)
  (:documentation "test local replication of current db to new db using string identifier")
  db-replicate-local-1
  (let ((db *couchdb*))
    (with-temp-db
      (let ((target *couchdb*)
            (*couchdb* db))
        (ensure (document-property :|ok| (replicate target)))))))

(addtest (clouchdb-replication-tests)
  (:documentation "test local replication of current db to new db using string identifiers")
  db-replicate-local-2
  (ensure (document-property :|ok| 
                             (let ((source *couchdb*))
                               (with-temp-db
                                 (let ((target *couchdb*))
                                   (replicate (db-name target) 
                                              :source (db-name source))))))))

(addtest (clouchdb-replication-tests)
  (:documentation 
   "test local and remote replication of current db to new db using db and string identifiers")
  db-replicate-mixed-1
  (ensure (document-property :|ok| 
                             (let ((source *couchdb*))
                               (with-temp-db
                                 (let ((target *couchdb*))
                                   (replicate target
                                              :source (db-name source))))))))

(addtest (clouchdb-replication-tests)
  (:documentation 
   "test remote API replication of current db to new db using database identifiers")
  db-replicate-dbs
  (ensure (document-property :|ok| 
                             (let ((source *couchdb*))
                               (with-temp-db
                                 (let ((target *couchdb*))
                                   (replicate target :source source)))))))

(addtest (clouchdb-replication-tests)
  (:documentation "test abilty to get document merge conflicts")
  db-replicate-doc-conflict
  (let ((db1 *couchdb*))
    (with-temp-db
      (create-document '((name . "foo")) :id "x")
      (replicate db1)
      (put-document (set-document-property 
                     (get-document "x") :name "bar"))
      (let ((*couchdb* db1))
        (put-document (set-document-property 
                       (get-document "x") :name "baz")))
      (replicate db1)
      (replicate *couchdb* :source db1)
      (ensure (document-property :|_conflicts| (get-document "x" :conflicts t))))))

;;
;; View API Tests
;;

(deftestsuite clouchdb-view-tests () 
  ()
  (:dynamic-variables
   (*couchdb* (make-db :db *couchdb*)))
  (:setup
   (progn
     (set-connection :db (create-temp-db))
     (create-test-documents *people* :id-field :name)))
  (:teardown 
   (progn 
     (delete-db))))

(addtest (clouchdb-view-tests)
  (:documentation "Create an ad-hoc view and verify the returned count")
  ad-hoc-view-result
  (ensure-same (length (contains-property *people* :name :pval "marc"))
	       (document-property
		:|total_rows|
		(ad-hoc-view 
		 (ps (lambda (doc)
                       (with-slots (*NAME*) doc
                         (if (= *NAME* "marc")
                             (emit null *NAME*)))))))))

(addtest (clouchdb-view-tests)
  (:documentation "Create an ad-hock view that should return no results")
  ad-hoc-view-no-result
  (ensure-same 0 (document-property
		  :|total_rows|
		  (ad-hoc-view 
		   (ps (lambda (doc)
                         (if (= doc.name "marie")
                             (emit null doc.name))))))))

(addtest (clouchdb-view-tests)
  (:documentation "Ensure a view can be created")
  create-view-test1
  (ensure 
   (document-property 
    :|ok|
    (create-ps-view "friend"
                    (ps-view ("marie-view")
                      (defun map (doc)
                        (with-slots (*friends*) doc
                          (dolist (friend *friends*)
                            (if (= friend "marie")
                                (emit null doc))))))))))

(addtest (clouchdb-view-tests)
  (:documentation "Ensure a view can be created and deleted")
  create-view-test2
  (ensure 
   (document-property
    :|ok|
    (create-view "friend"
                 (ps-view ("marie-view")
                   (defun map (doc)
                     (with-slots (friends) doc
                       (dolist (friend friends)
                         (if (= friend "marie")
                             (emit null doc)))))))))
   (ensure (document-property :|ok| (delete-view "friend"))))
			     
(addtest (clouchdb-view-tests)
  (:documentation "Creating a view that already exists should report an error")
  create-view-test3
  (ensure 
   (document-property
    :|ok|
    (create-ps-view "friend"
                    (ps-view ("marie-view")
                      (defun map (doc)
                        (with-slots (*friends*) doc
                          (dolist (friend *friends*)
                            (if (= friend "marie")
                                (emit null doc)))))))))
  (ensure-same "conflict"
	       (document-property
		:|error|
		(create-ps-view "friend"
                                (ps-view ("marie-view")
                                  (defun map (doc)
                                    (with-slots (*friends*) doc
                                      (dolist (friend *friends*)
                                        (if (= friend "marie")
                                            (emit null doc))))))))))

(addtest (clouchdb-view-tests)
  (:documentation "Create a view and see if it can be queried")
  create-view-query-test1
  (ensure 
   (document-property 
    :|ok|
    (create-ps-view "friend"
                    (ps-view ("marie-view")
                      (defun map (doc)
                        (with-slots (*friends*) doc
                          (dolist (friend *friends*)
                            (if (= friend "marie")
                                (emit null doc)))))))))
  (ensure-same (document-property :|total_rows| (invoke-view "friend" "marie-view"))
 	       (length (contains-property *people* :friends :pval "marie"))))

(addtest (clouchdb-view-tests)
  (:documentation "Create a view and see if it can be queried with a key")
  create-view-keyquery-test1
  (ensure 
   (document-property 
    :|ok|
    (create-ps-view "friend"
                    (ps-view ("fname")
                      (defun map (doc)
                        (with-slots (*friends*) doc
                          (dolist (friend *friends*)
                            (emit friend doc))))))))
  (ensure-same (length (document-property 
			:|rows| (invoke-view "friend" "fname" :key "claire")))
 	       (length (contains-property *people* :friends :pval "claire"))))

(addtest (clouchdb-view-tests)
  (:documentation "Create a view and see if it can be queried with a complex key")
  create-view-keyquery-test2
  (ensure 
   (document-property 
    :|ok|
    (create-ps-view "views" 
                    (ps-view ("view")
                      (defun map (doc)
                        (with-slots (*city* *friends*) doc
                          (dolist (friend *friends*)
                            (emit (list *city* friend) doc))))))))
   (ensure-same 1 
                (length (document-property 
                         :|rows| 
                         (invoke-view "views" "view" :key '("boston" "charles"))))))

(addtest (clouchdb-view-tests)
  (:documentation "Create a view and see if it can be queried with a complex start and end key")
  create-view-keyquery-test3
  (ensure 
   (document-property 
    :|ok|
    (create-ps-view "views" 
                    (ps-view ("view")
                      (defun map (doc)
                        (with-slots (*city* *friends*) doc
                          (dolist (friend *friends*)
                            (emit (list *city* friend) doc))))))))
   (ensure-same 1 
                (length (document-property 
                         :|rows| 
                         (invoke-view "views" 
                                      "view" 
                                      :start-key '("boston" "c")
                                      :end-key '("boston" "d"))))))


(defun run-all-tests ()
  (dolist (suite '(clouchdb-general-tests
                   clouchdb-db-admin-tests
		   clouchdb-doc-api-tests
		   clouchdb-view-tests
                   clouchdb-replication-tests))
    (format t "~S~%" (run-tests :suite suite))))

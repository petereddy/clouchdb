;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLOUCHDB-TESTS; Base: 10 -*-
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
  (:use :cl :parenscript :stefil :clouchdb))

(in-package :clouchdb-tests)

(in-root-suite)
(defsuite* test-all)

(defsuite* test-non-db)

;; Ensure (document-property ..) gets correct property name value from document
(deftest document-property-test ()
   (let ((doc '((:NAME . "Value1") (:|Name| . "Value2") (:|NaMe| . "Value3"))))
     (is (reduce #'(lambda (a b) (and a b))
                        (mapcar #'(lambda (e)
                                    (equal (cdr (assoc (car e) doc))
                                           (document-property (car e) doc)))
                                doc)))))

;; Ensure connection information is correctly passed with-connection
(deftest clouchdb-with-connection0 ()
  (with-connection (:name "wc-name" :port "3434" :protocol "https" :user "wc-user" 
                          :password "wc-pass" :document-fetch-fn #'clouchdb::delete-db
                          :document-update-fn #'clouchdb::create-db)
    (is (equal (db-name *couchdb*) "wc-name"))
    (is (equal (db-port *couchdb*) "3434"))
    (is (equal (clouchdb::db-protocol *couchdb*) "https"))
    (is (equal (db-user *couchdb*) "wc-user"))
    (is (equal (db-password *couchdb*) "wc-pass"))))

;; Ensure user can be set to nil
(deftest clouchdb-with-connection1 ()
  (with-connection (:user "wc-user")
    (with-connection (:user nil)
      (is (null (db-user *couchdb*))))))

;;
;; Database API Tests
;;
;; Tests ClouchDb APIs that create and delete databases and get
;; CouchDb server information.
;;

;;
;; General tests that do not require a db connection
;;

(defsuite* clouchdb-general-tests)

;;
;; (document-property) tests
;;

;; Test accessing document property with property list
(deftest general-tests-document-property-single ()
  (is 
   (= 2 (document-property ':one
                           '((:one . 2) (:a . ((:b . ((:c . "found"))))))))))

;; Test accessing document property with single property list
(deftest general-tests-document-property-single-list ()
  (is 
   (= 2
      (document-property '(:one)
                         '((:one . 2) (:a . ((:b . ((:c . "found"))))))))))

;; Test accessing document property with property list
(deftest general-tests-document-property-list-access ()
  (is
   (equal "found"
          (document-property '(:a :b :c)
                             '((:one . 2) (:a . ((:b . ((:c . "found"))))))))))

;;
;; (setf document-property) tests
;;

;; Set existing document property with single property
(deftest general-tests-document-property-modify ()
  (let ((doc '((:one . 2) (:a . ((:b . ((:c . "not searched for"))))))))
    (is
     (equal "found" 
            (document-property :a 
                               (setf (document-property :a doc) "found"))))))

;; "Set existing document property with single element list property
(deftest general-tests-document-property-single-list-modify ()
  (let ((doc '((:one . 2) (:a . ((:b . ((:c . "not searched for"))))))))
    (is
     (equal "found" 
            (document-property 
             :a 
             (setf (document-property '(:a) doc) "found"))))))

;; Add top level document property with (setf (document-property))")
(deftest general-tests-document-property-add-top-level ()
  (let ((doc '((:one . 1) (:two . 2))))    
    (is (= 3 (document-property 
                     :three 
                     (setf (document-property :three doc) 3))))))

;; Add document property list to document.
(deftest  general-tests-document-property-add-property-list ()
  (let ((doc '((:one . 1) (:two . 2)))
        (properties '(:three :four)))
    (is (= 4 
                  (document-property 
                   properties
                   (setf (document-property properties doc) 4))))))

;; Replace document property with nested property list.
(deftest 
  general-tests-document-property-replace-property-list ()
  (let ((doc '((:one . 1) (:two . 2) (:three . 3)))
        (properties '(:three :four)))
    (is (= 4 
                  (document-property 
                   properties
                   (setf (document-property properties doc) 4))))))

;; Change nested document property.
(deftest general-tests-document-property-replace-property-list1 ()
  (let ((doc '((:one . 1) (:two . ((:four . ((:nine . 8))))) (:three . 3)))
        (properties '(:two :four :nine)))
    (is (= 9
                  (document-property 
                   properties
                   (setf (document-property properties doc) 9))))))

;; Truncate deep document property list.
(deftest general-tests-document-property-replace-property-list2 ()
  (let ((doc '((:one . 1) (:two . ((:four . ((:nine . 9))))) (:three . 3)))
        (properties '(:two :four)))
    (is (= 4
                  (document-property 
                   properties
                   (setf (document-property properties doc) 4))))))

;; Create document with single property (setf (document-property))
(deftest general-tests-document-property-create-property ()
  (is (= 44 
                (document-property 
                 :value 
                 (setf (document-property ':value nil) 44)))))

;; Create document with property list (setf (document-property))
(deftest  general-tests-document-property-create-propert-list1 ()
  (is (= 44 
                  (document-property 
                   :value 
                   (setf (document-property '(:value) nil) 44)))))

;; Create document with property list (setf (document-property))
(deftest general-tests-document-property-create-propert-list-multi ()
  (is (= 44 
                (document-property 
                 '(:some :nested :value)
                 (setf (document-property '(:some :nested :value) nil) 44)))))

;; ;; 
;; ;; 
;; ;; 

;; Test case-encoded field name functions
(deftest general-tests-case-encoded ()
  (is (equal "lowercase" (as-field-name-string (as-keyword-symbol "lowercase"))))
  (is (equal "MixedCase" (as-field-name-string (as-keyword-symbol "MixedCase"))))
  (is (equal "Mixed-Case-Hyphen" 
               (as-field-name-string (as-keyword-symbol "Mixed-Case-Hyphen"))))
  (is (equal "UPPER-CASE" 
               (as-field-name-string (as-keyword-symbol "UPPER-CASE")))))

;; test keyword-assocp for positive match
(deftest general-tests-keword-assocp-positivie ()
  (is (clouchdb::keyword-assocp '(:key . "value")))
  (is (clouchdb::keyword-assocp '(:key . 3)))
  (is (clouchdb::keyword-assocp '(:key . 'value)))
  (is (clouchdb::keyword-assocp '(:key . (1 2 3))))  
  (is (clouchdb::keyword-assocp '(:key . ((1 2 3)))))
  (is (clouchdb::keyword-assocp '(:key . ((:a . "aye") (:b . "bee"))))))

;; test keyword-assocp for positive match
(deftest general-tests-keword-assocp-negative ()
  (is (not (clouchdb::keyword-assocp '())))
  (is (not (clouchdb::keyword-assocp '(3 4))))
  (is (not (clouchdb::keyword-assocp '(abe lincolin)))))

;; test assoclp function for positive match
(deftest general-tests-assoclp-positive ()
  (is (clouchdb::assoclp '((:a . b) (:c . "dee"))))
  (is (clouchdb::assoclp '((:a (1 2 3)))))
  (is (clouchdb::assoclp '((:a . nil) (:b . "froth")))))

;; test assoclp function for non-matches
(deftest general-tests-assoclp-negative ()
  (is (not (clouchdb::assoclp '())))
  (is (not (clouchdb::assoclp '(:a . 3))))
  (is (not (clouchdb::assoclp '(:a (1 2 3)))))
  (is (not (clouchdb::assoclp '(:a (:b . "sea")))))
  (is (not (clouchdb::assoclp '(:a ((:b . "sea") (:d . "e")))))))
  ;;(is (not (clouchdb::assoclp '((:aye :bee :sea))))
  ;;(is (not (clouchdb::assoclp '((:aye :bee (:a . 3) (:b . "froth"))))))

;;
;; *document0* query tests
;;

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

(deftest general-tests-document0-query ()
  (is (= 2 (car (query-document '(:|total_rows|) *document0*))))
  (is (= 2 (length (car (query-document '(:|rows|) *document0*)))))
  (is (= 2 (length (query-document '(:|rows| :|value|) *document0*))))
  (is (progn
            (let ((res (query-document '(:|rows| :|value| :|_id|) *document0*)))
              (and (find "id2a" res :test #'equal)
                   (find "id1a" res :test #'equal)
                   (eql 2 (length res))))))
  (is (= 2 (length  (query-document '(:|rows| :|value| :acl) *document0*))))
  (is (progn
            (let ((res (query-document '(:|rows| :|value| :acl :read) *document0*)))
              (and (eql 2 (length res))
                   (find "reader1" (car res) :test #'equal)
                   (find "reader3" (car res) :test #'equal)
                   (find "reader1" (second res) :test #'equal)
                   (find "reader2" (second res) :test #'equal))))))
  
;; document0* query wildcard tests
(deftest general-tests-document0-query-wildcard-top ()
  (is (= 2 (car (query-document '(:|total_rows|) *document0*)))
  (is (= 2 (length (car (query-document '(:|rows|) *document0*)))))
  (is (= 2 (length (query-document '(:|rows| :|value|) *document0*))))
  (is (progn
        (let ((res (query-document '(:** :|_id|) *document0*)))
          (and (find "id2a" res :test #'equal)
               (find "id1a" res :test #'equal)
               (eql 2 (length res))))))
  (is (= 2 (length  (query-document '(:** :acl) *document0*))))
  (is (progn
        (let ((res (query-document '(:** :read) *document0*)))
          (and (eql 2 (length res))
               (find "reader1" (car res) :test #'equal)
               (find "reader3" (car res) :test #'equal)
               (find "reader1" (second res) :test #'equal)
               (find "reader2" (second res) :test #'equal)))))
  (is (progn
        (let ((res (query-document '(:|rows| :** :read) *document0*)))
          (and (eql 2 (length res))
               (find "reader1" (car res) :test #'equal)
               (find "reader3" (car res) :test #'equal)
               (find "reader1" (second res) :test #'equal)
               (find "reader2" (second res) :test #'equal)))))))

;; *document0* query wildcard tests
(deftest general-tests-document0-query-wildcard-middle ()
  (is (progn
        (let ((res (query-document '(:|rows| :** :|_id|) *document0*)))
          (and (find "id2a" res :test #'equal)
               (find "id1a" res :test #'equal)
               (eql 2 (length res))))))
  (is (= 2 (length  (query-document '(:|rows| :** :acl) *document0*))))
  (is (progn
        (let ((res (query-document '(:|rows| :** :read) *document0*)))
          (and (eql 2 (length res))
               (find "reader1" (car res) :test #'equal)
               (find "reader3" (car res) :test #'equal)
               (find "reader1" (second res) :test #'equal)
               (find "reader2" (second res) :test #'equal))))))

;;
;;
;;

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


;; *people* query tests
(deftest general-tests-people-query ()
  (is (progn
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

(defsuite* clouchdb-db-admin-tests)

;; Look for the welcome message and version info from server
(deftest generic-server-info-query ()
  (is (equal "Welcome" (document-property :|couchdb| (get-couchdb-info))))
  (is (document-property :|version| (get-couchdb-info))))

;; Ensure get-db-info reports non-existant databases
(deftest db-non-existance-test ()
  (let ((*couchdb* (make-db :name (create-temp-db-name))))
    (is (equal "not_found" (document-property :|error| 
                                              (get-db-info))))
    (is (equal "no_db_file" (document-property :|reason|  (get-db-info))))))

;; Create a database and ensure it gets created
(deftest db-creation-test ()
  (with-temp-db
    (is (equal (document-property :|db_name| (get-db-info))
               (db-name *couchdb*)))
    (is (equal (document-property :|db_name| (get-db-info :db *couchdb*))
               (db-name *couchdb*)))
    (is (= 0 (document-property :|doc_count| (get-db-info :db *couchdb*))))
    (is (= 0 (document-property :|update_seq| (get-db-info :db *couchdb*))))))

;; Be sure differnt get-db-info function invocations return the same results
(deftest get-db-info-test ()
  (with-temp-db
    (is (tree-equal (get-db-info) (get-db-info :db *couchdb*) 
                    :test #'equal))
    (is (tree-equal (get-db-info) (get-db-info :db (db-name *couchdb*))
                        :test #'equal))))

;; Make sure deleting a nonexistant db generates an error
(deftest db-delete-non-existant-db ()
  (signals db-does-not-exist (delete-db :db (create-temp-db-name))))

;; Make sure deleting a nonexistant db error is ignoreable
(deftest db-ignore-delete-non-existant-db ()
  (is (document-property 
       :|error| 
       (delete-db :if-missing :ignore :db (create-temp-db-name)))))

;; Creating a db that already exists is an error
(deftest db-create-existant-db ()
  (signals db-already-exists 
    (with-temp-db (create-db))))

;; Creating a db that already exists is an error
(deftest db-create-existant-db-name ()
  (signals db-already-exists 
    (with-temp-db (db-name (create-db :db (db-name *couchdb*))))))

;; Ignore the duplicate db create error
(deftest db-ignore-create-existant-db ()
  (is (document-property :|ok|
                         (with-temp-db
                           (create-db :if-exists :ignore)))))

;; Recreate option for create-db on existing db
(deftest db-recreate-db ()
  (is (document-property :|ok|
                         (with-temp-db
                           (create-db :if-exists :recreate)))))

;; Recreate option for create-db on non-existant db
(deftest db-recreate-nonexistant-db ()
  (with-temp-db
    (is (document-property :|ok| (create-db :if-exists :recreate)))))

;; Test handling of illegal db name
(deftest db-create-illegal-db-name ()
  (signals illegal-database-name
    (db-name (create-db :db (make-db :name "FOO")))))

;; Initate compaction
(deftest db-compact ()
  (with-temp-db (is (document-property :|ok| (compact-db)))))

;; compaction specifying db name
(deftest db-compact-name ()
  (with-temp-db
    (is (document-property :|ok| (compact-db :db (db-name *couchdb*))))))

;; compaction specifying structure
(deftest db-compact-db-struct ()
  (with-temp-db
    (is (document-property :|ok| (compact-db :db *couchdb*)))))

;;
;; Document API Tests
;;

(defsuite* clouchdb-doc-api-tests)

;;
;; Test helper functions
;;

(defun create-test-documents (data &key (id-field nil))
  "Add data to database. If id-field is provided then that value
identifies the key of the value in the data that should be used for
the document ID. If not specified the database will generate a unique
key."
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

;; Ensure temporary db for these tests is succesfully created.
(deftest empty-test ()
  (with-temp-db
    (is (equal (document-property :|db_name| (get-db-info)) 
               (db-name *couchdb*)))))

;; Create a document with create-document
(deftest create-document-auto-id ()
  (with-temp-db
    (is (document-property :|ok| (create-document '((:a . "test")))))))

;; Create document with create-document, specify document ID
(deftest create-document-specified-id ()
  (with-temp-db 
    (is (document-property 
         :|ok| 
         (create-document '((:a . "test")) :id "specified")))))

;; Create a document with a duplicate ID and ensure revision conflict
(deftest create-document-specified-id-conflict ()
  (with-temp-db
    (is (document-property :|ok| (create-document '((:a . "test")) 
                                                  :id "specified")))
    (signals id-or-revision-conflict
      (create-document '((:a "test")) :id "specified"))))

;; Create a document with put-document
(deftest put-document-create-id ()
  (with-temp-db
    (is (document-property :|ok| (put-document '((:a "test")) :id "specified")))))

;; Create a document with put-document with no ID (error)
(deftest put-document-create-no-id ()
  (with-temp-db
    (signals id-missing (put-document '((:a "test"))))))

;; Get a non-existant document (error)
(deftest get-non-existant-document ()
  (with-temp-db
    (signals document-missing (get-document "does-not-exist"))))

;; Get a non-existant document, ignore error
(deftest get-non-existant-document-ignore1 ()
  (with-temp-db
    (is (equal nil (get-document "does-not-exist" :if-missing :ignore)))))

;; Get a non-existant document, use missing value
(deftest get-non-existant-document-missing-value ()
  (with-temp-db
    (is (equal "hi" (get-document "does-not-exist" :if-missing "hi")))))

;; Test revision info
(deftest get-document-revision-info ()
  (with-temp-db
    (is (= 11 (progn 
                ;; Create a document with one field, f, and revize it a
                ;; lot
                (put-document '((:f . "empty")) :id "revizedalot")
                (dotimes (i 10)
                  (let ((doc (get-document "revizedalot")))
                    (setf (document-property :f doc) i)
                    (put-document doc)))
                ;;
                (let ((docinf (get-document "revizedalot" :revision-info t)))
                  (length (document-property :|_revs_info| docinf))))))))
	    
;; Copy document, creating destination with source ID
(deftest copy-document-create-dest-id ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (copy-document "source" "dest"))))

;; Copy document, creating destination with source document
(deftest copy-document-create-dest-doc ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (copy-document (get-document "source") "dest"))))

;; Copy document, overwrite destination by specifying revision
(deftest copy-document-overwrite-dest-1 ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (create-document '((:dest . "data")) :id "dest")) 
    (is (copy-document "source" "dest" 
                       :revision (document-revision (get-document "dest"))))))

;; Copy document, overwrite destination by specifying current revision
(deftest copy-document-overwrite-dest-2 ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (create-document '((:dest . "data")) :id "dest")) 
    (is (copy-document "source" "dest" :revision :current))))

;; Copy document, overwrite destination by specifying dest document
(deftest copy-document-overwrite-dest-3 ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (create-document '((:dest . "data")) :id "dest")) 
    (is (copy-document "source" (get-document "dest")))))

;; Copy document, overwrite destination by specifying current revision
(deftest copy-document-overwrite-revision-conflict ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (create-document '((:dest . "data")) :id "dest")) 
    (signals id-or-revision-conflict (copy-document "source" "dest"))))

;; Copy document, overwrite destination by specifying current revision
(deftest copy-document-source-id-missing ()
  (with-temp-db
    (is (create-document '((:source . "data")) :id "source"))
    (is (create-document '((:dest . "data")) :id "dest")) 
    (signals doc-error (copy-document "doesnotexist" "dest"))))

;; Delete a document by ID
(deftest delete-document-by-id ()
  (with-temp-db
    (is (document-property :|ok| (create-document '((:a "test")) :id "specified")))
    (is (document-property :|ok| (delete-document "specified")))))

;; Delete a document by ID and revision
(deftest delete-document-by-id-and-revision ()
  (with-temp-db
    (is (progn
          (create-document '((:a . "document")) :id "specified")
          (let ((doc (get-document "specified")))
            (document-property :|ok| 
                               (delete-document (document-id doc)
                                                :revision 
                                                (document-revision doc))))))))

;; Delete a document by document
(deftest delete-document-by-document ()
  (with-temp-db
    (is (progn
          (create-document '((:a . "document")) :id "polly")
          (document-property :|ok| 
                             (delete-document (get-document "polly")))))))

;; Delete a non-existant document
(deftest delete-document-bad-id ()
  (with-temp-db
    (signals document-missing (delete-document "specified"))))

;; Delete a non-existant document and ignore error
(deftest delete-document-missing-ignore ()
  (with-temp-db
    (is (equal nil (delete-document "specified" :if-missing :ignore)))))

;; Delete a non-existant document and return custom value
(deftest delete-document-missing-custom-value ()
  (with-temp-db
    (is (equal "hi" (delete-document "specified" :if-missing "hi")))))

;; Add a bunch of documents and ensure they get created
(deftest create-document-test1 ()
  (with-temp-db
    (is (= (length (create-test-documents *people* :id-field :name))
	       (length *people*)))
    (is (ensure-ids *people* :name))))

;; Test get-document for existing document
(deftest get-document-test ()
  (with-temp-db
    (is (document-property :|ok| (create-document '((:a . "test")) :id "test")))
    (is (equal (document-property :|_id| (get-document "test")) "test"))))

;; Update a document property
(deftest update-document-test1 ()
  (with-temp-db
    (is (create-test-documents *people* :id-field :name))
    (is (progn
          (let ((d (get-document "peter")))
            (setf (document-property :city d) "San Francisco")
            (put-document d)
            (equal "San Francisco"
                   (document-property :city (get-document "peter"))))))))

;; Add a property to an existing document
(deftest add-document-property-test1 ()
  (with-temp-db
    (is (create-test-documents *people* :id-field :name))
    (is (document-property :|ok| (put-document (cons '(:handsome . "false")
                                                     (get-document "peter")))))))

;; Test bulk updates of documents
(deftest bulk-update-1 ()
  (with-temp-db
    (is (create-test-documents *people* :id-field :name))
    (is (progn
          (let ((docs))
            (dolist (di (document-property :|rows| (get-all-documents)))
              (push (cons '(:new-field . "New Value") 
                          (get-document (document-property :|id| di)))
                    docs))
            (is (not (document-property :|error| (bulk-document-update docs))))
            (block test
              (loop for di in (document-property :|rows| (get-all-documents)) do
                   (if (not (equal "New Value" 
                                   (document-property :new-field
                                                      (get-document 
                                                       (document-property :|id| di)))))
                       (return-from test nil)))
              t))))))

;; Test document ID encoding
(deftest encode-document-id ()
  (with-temp-db
    (is (document-property :|ok| 
                               (create-document '((:a "test")) 
                                                :id "http://google.com")))
    (is (equal (document-id (get-document "http://google.com")) 
               "http://google.com"))))

;; Test UUID default function invocation
(deftest uuid-default-test ()
  (with-temp-db
    (is (equal 1 (length (document-property :|uuids| (get-uuids)))))))

;; Test UUID default function invocation
(deftest uuid-count-test ()
  (with-temp-db
    (is (equal 3 (length (document-property :|uuids| (get-uuids :count 3)))))))

;; Test encoding and decoding of utf-8 document IDs
(deftest encode-document-utf-8-ids ()
  (with-temp-db
    (is
     (progn
       (let ((ids '("Ångström Café" "σπασμένα" "我能吞下玻璃而不伤身体")))
         (reduce #'(lambda (a b) (and a b))
                 (mapcar #'(lambda (id)
                             (and (document-property :|ok| (create-document nil :id id))
                                  (equal id (document-property :|_id| 
                                                               (get-document id)))))
                         ids)))))))

;; Test document content encoding by creating a document with a field
;; for a variety of languages, then fetching that document and
;; comparing the fecthed data with the source data
(deftest encode-document-contents-glass-eating ()
  (with-temp-db
    (is 
     (progn
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
                                doc)))))))))

;;
;; Attachments 
;;

(defsuite* clouchdb-attachments)

;; Test adding attachment 0
(deftest add-attachment-0 ()
  (with-temp-db
    (is (document-property :|ok|
                           (add-attachment "doc" 
                                           (pathname "tests.lisp"))))
    (let ((attachments (attachment-list "doc")))
      (is (= 1 (length attachments)))
      (is (equal "tests.lisp" (attachment-name (car attachments)))))))

;; Test adding attachment using name other than file name
(deftest add-attachment-1 ()
  (with-temp-db
    (is (document-property :|ok|
                           (add-attachment "doc" 
                                           (pathname "tests.lisp")
                                           :name "something.lisp")))
    (let ((attachments (attachment-list "doc")))
      (is (= 1 (length attachments)))
      (is (equal "something.lisp" (attachment-name (car attachments)))))))

;; Test adding multiple attachments
(deftest add-attachment-2 ()
  (with-temp-db
    (is (document-property :|ok|
                           (add-attachment "doc" 
                                           (pathname "tests.lisp")
                                           :name "something.lisp")))
    (is (document-property :|ok|
                           (add-attachment "doc" 
                                           (pathname "tests.lisp")
                                           :name "something.else")))
    (let ((attachments (attachment-list "doc")))
      (is (= 2 (length attachments)))
      (is (find :|something.lisp| attachments :key #'car))
      (is (find :|something.else| attachments :key #'car)))))

;; Test creating attachment with stream
(deftest add-attachment-stream ()
  (with-temp-db
    (is (document-property :|ok|
                               (add-attachment "doc" 
                                               (pathname "tests.lisp"))))
    (is (document-property 
             :|ok| 
             (with-attachment (stream "doc" "tests.lisp")
               (add-attachment "doc2" stream :name "tests2.lisp"))))
    (let ((attachments (attachment-list "doc2")))
      (is (= 1 (length attachments)))
      (is (find :|tests2.lisp| attachments :key #'car)))))

;; Make shure conflicts don't appear when they shouldn't
(deftest test-no-conflicts ()
  (with-temp-db
    (is (create-document '((:name . "hi")) :id "hi"))
    (is (null (document-property :|_conflicts| (get-document "hi"))))))

;; 
;; Replication Tests.
;;

(defsuite* clouchdb-replication-tests)

;; (deftestsuite clouchdb-replication-tests () 
;;   ()
;;   (:dynamic-variables
;;    (*couchdb* (make-db)))
;;   (:setup
;;    (progn
;;      (set-connection :db (create-temp-db))
;;      (create-test-documents *people* :id-field :name)))
;;   (:teardown 
;;    (progn 
;;      (delete-db)
;;      (set-connection :name "default"))))

;; test local replication of first db to new second db
(deftest db-replicate-local-1 ()
  (with-temp-db
    (let ((db *couchdb*))
      (create-test-documents *people* :id-field :name)
      (with-temp-db
        (let ((target *couchdb*)
              (*couchdb* db))
          (is (document-property :|ok| (replicate target))))))))

;; test local replication of current db to new db using string identifiers
(deftest db-replicate-local-2 ()
  (with-temp-db
    (is (document-property :|ok| 
                           (let ((source *couchdb*))
                             (with-temp-db
                               (let ((target *couchdb*))
                                 (replicate (db-name target) 
                                            :source (db-name source)))))))))

;; test local and remote replication of current db to new db using db
;; and string identifiers
(deftest db-replicate-mixed-1 ()
  (with-temp-db
    (let ((source *couchdb*))
      (with-temp-db
        (let ((target *couchdb*))
          (replicate target :source (db-name source)))))))

;; test remote API replication of current db to new db using database
;; identifiers
(deftest db-replicate-dbs ()
  (with-temp-db
    (is (document-property :|ok| 
                           (let ((source *couchdb*))
                             (with-temp-db
                               (let ((target *couchdb*))
                                 (replicate target :source source))))))))

;; test abilty to get document merge conflicts
(deftest db-replicate-doc-conflict ()
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
      (is (document-property :|_conflicts| (get-document "x" :conflicts t))))))

;;
;; View API Tests
;;

(defsuite* clouchdb-view-tests)
  ;; ()
  ;; (:dynamic-variables
  ;;  (*couchdb* (make-db :db *couchdb*)))
  ;; (:setup
  ;;  (progn
  ;;    (set-connection :db (create-temp-db))
  ;;    (create-test-documents *people* :id-field :name)))
  ;; (:teardown 
  ;;  (progn 
  ;;    (delete-db))))

;; Create an ad-hoc view and verify the returned count
;; (deftest ad-hoc-view-result ()
;;   (with-temp-db
;;     (create-test-documents *people* :id-field :name)
;;     (is (equal (length (contains-property *people* :name :pval "marc"))
;;                (document-property
;;                 :|total_rows|
;;                 (ad-hoc-view 
;;                  (ps (lambda (doc)
;;                        (with-slots (*NAME*) doc
;;                          (if (= *NAME* "marc")
;;                              (emit null *NAME*)))))))))))

;; Create an ad-hock view that should return no results
(deftest ad-hoc-view-no-result ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is (= 0 (document-property
              :|total_rows|
              (ad-hoc-view 
               (ps (lambda (doc)
                     (if (= doc.name "marie")
                         (emit null doc.name))))))))))

;; Ensure new views can be created
(deftest create-view-test1 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is
     (document-property 
      :|ok|
      (create-ps-view "friend"
                      (ps-view ("marie-view")
                        (defun map (doc)
                          (with-slots (*friends*) doc
                            (dolist (friend *friends*)
                              (if (= friend "marie")
                                  (emit null doc)))))))))))

;; Ensure a view can be created and deleted
(deftest create-view-test2 ()
  (with-temp-db
    (is
     (document-property
      :|ok|
      (create-view "friend"
                   (ps-view ("marie-view")
                     (defun map (doc)
                       (with-slots (friends) doc
                         (dolist (friend friends)
                           (if (= friend "marie")
                               (emit null doc)))))))))
    (is (document-property :|ok| (delete-view "friend")))))
			     
;; Creating a view that already exists should report an error
(deftest create-view-test3 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is 
     (document-property
      :|ok|
      (create-ps-view "friend"
                      (ps-view ("marie-view")
                        (defun map (doc)
                          (with-slots (*friends*) doc
                            (dolist (friend *friends*)
                              (if (= friend "marie")
                                  (emit null doc)))))))))
    (signals id-or-revision-conflict
      (create-ps-view "friend"
                      (ps-view ("marie-view")
                        (defun map (doc)
                          (with-slots (*friends*) doc
                            (dolist (friend *friends*)
                              (if (= friend "marie")
                                  (emit null doc))))))))))

;; Create a view and see if it can be queried
(deftest create-view-query-test1 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is
     (document-property 
      :|ok|
      (create-ps-view "friend"
                      (ps-view ("marie-view")
                        (defun map (doc)
                          (with-slots (*friends*) doc
                            (dolist (friend *friends*)
                              (if (= friend "marie")
                                  (emit null doc)))))))))
    (is (equal (document-property :|total_rows| (invoke-view "friend" "marie-view"))
               (length (contains-property *people* :friends :pval "marie"))))))

;; Create a view and see if it can be queried with a key
(deftest create-view-keyquery-test1 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is (document-property 
         :|ok|
         (create-ps-view "friend"
                         (ps-view ("fname")
                           (defun map (doc)
                             (with-slots (*friends*) doc
                               (dolist (friend *friends*)
                              (emit friend doc))))))))
    (is (= (length (document-property 
                    :|rows| (invoke-view "friend" "fname" :key "claire")))
           (length (contains-property *people* :friends :pval "claire"))))))

;; Create a view and see if it can be queried with a complex key
(deftest create-view-keyquery-test2 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is (document-property 
         :|ok|
         (create-ps-view "views" 
                         (ps-view ("view")
                           (defun map (doc)
                             (with-slots (*city* *friends*) doc
                               (dolist (friend *friends*)
                                 (emit (list *city* friend) doc))))))))
    (is (= 1 
           (length (document-property 
                    :|rows| 
                    (invoke-view "views" "view" :key '("boston" "charles"))))))))

;; Create a view and see if it can be queried with a complex start and end key
(deftest create-view-keyquery-test3 ()
  (with-temp-db
    (create-test-documents *people* :id-field :name)
    (is
     (document-property 
      :|ok|
      (create-ps-view "views" 
                      (ps-view ("view")
                        (defun map (doc)
                          (with-slots (*city* *friends*) doc
                            (dolist (friend *friends*)
                              (emit (list *city* friend) doc))))))))
    (is (= 1 (length (document-property 
                      :|rows| 
                      (invoke-view "views" 
                                   "view" 
                                   :start-key '("boston" "c")
                                   :end-key '("boston" "d"))))))))

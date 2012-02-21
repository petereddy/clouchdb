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

(defpackage :clouchdb-examples
  (:use :cl :parenscript :clouchdb))

(in-package :clouchdb-examples)

;;
;;
;;

(defun example1 ()
  "Create a database, populate it with a single document. Retrieve
that document and change a property in it, then update it in the
database."
  ;; Do everything in the context of a database named "example1"
  (with-connection (:name "example1")
    ;; Create the "example1" database, if it already exists, delete it
    ;; and recreate it
    (create-db :if-exists :recreate)
    ;; Create a document with ID of "haussmann"
    (create-document '(;; Fields with scalar values:
		       (:name . "Georges-Eugène Haussmann") 
		       (:aka . "Baron Haussmann")
		       (:born . "1809-03-27")
		       (:died . "1891-01-11"))
		     :id "haussmann")
    ;; Retrieve document from the server..
    (let ((doc (get-document "haussmann")))
      ;; Change some property of the document
      (setf (document-property :aka doc) "The Hauss-man")
      ;; Put the updated document back
      (put-document doc))))

;;
;;
;;

(defun example2 ()
  "Create a database, populate it with some documents having various
structures, create a view based on a field common to all
documents (the 'tags' field), and then query those documents using the
view and print the results."
  (with-connection (:name "example2")
    ;; (Re)create database "example2"
    (create-db :if-exists :recreate)
    ;; A simple example document using an auto-generated ID supplied
    ;; by CouchDb:
    (create-document '(;; Field with associated scalar value:
		       (:|name| . "wine") 
		       ;; Field with array value:
		       (:|tags| . ("beverage" "fun" "alcoholic"))))
    ;; A document with somewhat different structure, and that uses a
    ;; specified ID (does not use auto-generated ID):
    (create-document '((:|first-name| . "Claude")
		       (:|last-name| . "Debussy")
		       (:|tags| . ("composer" "french" "impressionist" "european")))
		     ;; Specify an ID for this document:
		     :id "cdebussy")
    ;; Strings may be used for field names instead of symbols when
    ;; submitting documents. Fetched documents will always have
    ;; symbols for field names regardless of how they were created.
    (create-document '((:|name| . "Czech Republic")
		       (:|tags| . ("country" "european"))
		       (:|motto| . "Truth prevails")
		       (:|demographics| . ((:|population| . 10230000)
                                         ;; A nested map property:
                                         (:|religion| . ((:|Agnostic| . 0.59)
                                                         (:|Roman Catholic| . 0.26)
                                                         (:|Protestant| . 2.5)))
                                         (:|Political System| . "democracy"))))
		     :id "czechrepublic")
    ;; Create a persistant view that retrieves documents by their
    ;; tags. Views can also be queried with a browser at:
    ;; http://<host>:<port>/_view/<document-id>/<view-name>?key="<value>"
    ;; and edited in the HTML UI as a normal document.
    (create-ps-view
     ;; The view's document ID (views are just special documents):
     "tags"
     (ps-view ("tag") ; <-- the view name
       ;; The view definition in parenscript.
       (defun map (doc)
         (with-slots (tags) doc
           (dolist (tag tags)
             (emit tag doc))))))
    ;; Query the view defined above and print the results
    (let ((result (invoke-view "tags" "tag" :key "european")))
      (format t "found: ~S documents:~%" (length (document-property :|rows| result)))
      (dolist (doc (document-property :|rows| result))
	(format t "---~%ID: ~S~%document:~%~S~%" 
		(document-property :|id| doc) doc)))))

	
(defun example3 ()
  "Demonstrate the use of (query-document). Create a database,
populate it with some documents representing countries,
use (query-document) to operate on this data."
  (with-connection (:name "example3")
    (create-db :if-exists :recreate)
    (create-document '((:name . "Czech Republic")
		       (:tags . ("country" "european"))
		       (:motto . "Truth prevails")
		       (:demographics . ((:population . 10230000)
                                         (:capital . "Prague")
                                         (:eu-accession . "2004-05-01")
                                         (:president . "Václav Klaus")
                                         (:prime-minister . "Mirek Topolánek"))))
		     :id "czechrepublic")
    (create-document '((:name . "France")
		       (:tags . ("country" "european"))
		       (:motto . "Liberté, Égalité, Fraternité")
		       (:demographics . ((:population . 64473140)
                                         (:capital . "Paris")
                                         (:eu-accession . "1957-03-25")
                                         (:president . "Nicolas Sarkozy")
                                         (:prime-minister . "François Fillon"))))
		     :id "france")
    (create-document '((:name . "Italy")
		       (:tags . ("country" "european"))
		       (:motto . "Il Canto degli Italiani")
		       (:demographics . ((:population . 59337888)
                                         (:capital . "Rome")
                                         (:eu-accession . "1957-03-17")
                                         (:president . "Giorgio Napolitano")
                                         (:prime-minister . "Romano Prodi"))))
		     :id "italy")
    (create-document '((:name . "Turkey")
		       (:tags . ("country" "eurasian"))
		       (:motto . "Yurtta Sulh, Cihanda Sulh")
		       (:demographics . ((:population . 70586256)
                                         (:capital . "Ankara")
                                         (:prime-minister . "Recep Tayyip Erdoğan")
                                         (:president . "Abdulla Gül"))))
		     :id "turkey")
    ;; (get-all-documents) returns information about all documents in
    ;; the database:
    ;;
    ;; ((:|total_rows| . 3) 
    ;;  (:|offset| . 0) 
    ;;  (:|rows| 
    ;;    ((:|id| . "turkey") 
    ;;     (:|key| . "turkey") 
    ;;     (:|value| (:|rev| . "1081123848"))) 
    ;;    ((:|id| . "czechrepublic") 
    ;;     (:|key| . "czechrepublic") 
    ;;     (:|value| (:|rev| . "3192261183")))
    ;;    ((:|id| . "france") 
    ;;     (:|key| . "france") 
    ;;     (:|value| (:|rev| . "3199678281"))) 
    ;;    ((:|id| . "italy") 
    ;;     (:|key| . "italy")
    ;;     (:|value| (:|rev| . "2716436578")))))
    ;;
    ;; The first use of (query-document) below retuns a list of
    ;; document IDs (:|id|) values extracted from the alist values in
    ;; :|rows|:
    ;;
    ;; (query-document '(:|rows| :|id|) (get-all-documents))
    ;;  ==> ("turkey" "italy" "france" "czechrepublic")
    ;;
    ;; This list is then iterated and the country document ID is used
    ;; to fetch the actual country document using (get-document).
    ;;
    ;; The second use of (query-document) below operates on individual
    ;; country documents and recursively (:**) searches document
    ;; properties for the :eu-accession values:
    ;;
    ;; (query-document '(:** :eu-accession) (get-document "france"))
    ;; ==> ("1957-03-25")
    ;;
    ;; Alternatively this query could be expressed: 
    ;; '(:demographics :eu-accession).
    ;;
    ;; This will produce the following output:
    ;;
    ;; Country: turkey, EU accession: NIL
    ;; Country: italy, EU accession: 1957-03-17
    ;; Country: france, EU accession: 1957-03-25
    ;; Country: czechrepublic, EU accession: 2004-05-01
    ;;
    (dolist (country (query-document '(:|rows| :|id|) (get-all-documents)))
      (format t "Country: ~a, EU accession: ~a~%" country
              (car (query-document '(:** :eu-accession) (get-document country)))))
    ;;
    ;; It's possible to use a function in the query argument list of
    ;; (query-document). The following example will return a list of
    ;; all country presidents:
    ;;
    ;; ("Abdulla Gül" "Giorgio Napolitano" "Nicolas Sarkozy" "Václav Klaus")
    ;;
    ;; In this example the clouchdb function (get-document) is applied
    ;; to all ID values found matching the previous :|id| query
    ;; paramater, extracted from the (get-all-documents) results.  The
    ;; remainder of the query expression is applied to the function
    ;; result, which in this case is the fetched country document.
    ;;
    ;; Note the leading backquote in the query argument list
    ;;
    (query-document `(:|rows| :|id| ,#'get-document :** :president) (get-all-documents))))

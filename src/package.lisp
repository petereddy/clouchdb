;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

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

(defpackage :clouchdb
  (:use :cl :parenscript)
  (:export 
   :*couchdb*
   :*use-pool*
   :ad-hoc-view
   :add-attachment
   :add-shows-fns 
   :add-lists-fns
   :all-docs-by-seq
   :as-deleted-document
   :as-field-name-string
   :as-keyword-symbol
   :attachment-list
   :attachment-missing
   :attachment-name
   :bulk-document-update
   :changes
   :compact-db
   :copy-document
   :couchdb-document-properties
   :create-db
   :create-document
   :create-ps-view
   :create-temp-db
   :create-temp-db-name
   :create-view
   :database
   :db-already-exists
   :db-document-fetch-fn
   :db-document-update-fn
   :db-does-not-exist
   :db-existential-error
   :db-host
   :db-name
   :db-password
   :db-protocol
   :db-port
   :db-user
   :delete-attachment
   :delete-db
   :delete-document
   :delete-view
   :doc-error
   :document-as-hash
   :document-id
   :document-missing
   :document-properties
   :document-property
   :document-revision
   :document-to-json
   :encode-document
   :get-active-tasks
   :get-all-documents
   :get-attachment-name
   :get-attachment-stream
   :get-config
   :get-couchdb-info
   :get-db-info
   :get-document
   :get-uuids
   :id-missing
   :id-or-revision-conflict
   :illegal-database-name
   :invalid-input
   :invoke-view
   :json-to-document
   :json-string-or-stream-to-document
   :list-dbs
   :make-db
   :post-document
   :ps-view
   :defpsfun
   :put-document
   :query-document
   :replicate
   :save-attachment
   :set-connection
   :set-document-property
   :with-attachment
   :with-connection
   :with-temp-db))

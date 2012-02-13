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

;;; This is an implementation of a most recently used (MRU)
;;; cache. Random access to cached data is through a hashtable,

(proclaim '(inline cache-size rmcache))

;; Representation of cached value (hash table value)
(defstruct cached key value previous next)

;; The cache data structure
(defstruct cache 
  hashtable head tail 
  remove-notify-fn add-notify-fn
  (max-size 300 :type integer))

;;
;; Cache Helper Functions
;;

(defun rm-element (element cache)
  "Remove specified element from cache linked list, adjust cache head
and tail if apropriate."
  (if (cached-previous element)
      (setf (cached-next (cached-previous element)) (cached-next element))
      (setf (cache-head cache) (cached-next element)))
  (if (cached-next element)
      (setf (cached-previous (cached-next element)) (cached-previous element))
      (setf (cache-tail cache) (cached-previous element))))

(defun move-to-head (element cache)
  "Move specified cache element to head of cache doubly linked list."
  (unless (eq (cache-head cache) element)
    (rm-element element cache)
    (setf (cached-previous element) nil)
    (let ((old-head (cache-head cache)))
      (setf (cache-head cache) element)
      (setf (cached-next element) old-head)
      (if old-head
          (setf (cached-previous old-head) element)
          (setf (cache-tail cache) element)))))

(defun set-cache-top (element cache)
  "Set the element at the top of the cache. Used when adding new
element."
  (setf (cached-previous element) nil)
  (let ((head (cache-head cache)))
    (cond ((null head)
           (setf (cache-tail cache) element)
           (setf (cached-next element) nil))
          (t
           (setf (cached-previous head) element)
           (setf (cached-next element) head)))
    (setf (cache-head cache) element)))

(defun rm-cache (element cache)
  "Remove element from hashtable and linked list, call the remove
notification method if it has been specified."
  (rm-element element cache)
  (remhash (cached-key element) (cache-hashtable cache))
  (when (cache-remove-notify-fn cache)
    (funcall (cache-remove-notify-fn cache) 
             (cached-key element) (cached-value element) cache)))

;;
;; Public API
;;

(defun make-mru-cache (&key (size 100) (rehash-size 100)
                      (rehash-threshold 0.9) (max-size 300)
                      (test #'eql) remove-notify-fn)
  "Make a new MRU cache. Arguments are the same as for
make-hash-table, except for max-size, which limits the cache to a
specific size."
  (make-cache :hashtable (make-hash-table :size size 
                                          :test test
                                          :rehash-size rehash-size
                                          :rehash-threshold rehash-threshold)
              :remove-notify-fn remove-notify-fn
              :max-size max-size))

(defun get-cached (key cache &key (update-mru t))
  "Get cached value by key, move cached element to top of MRU list,
unless update-mru is false."
  (let ((element (gethash key (cache-hashtable cache))))
    (when (and element update-mru)
      (move-to-head element cache)
      (cached-value element))))

(defun cache-size (cache)
  "Return the number of elements in the cache."
  (hash-table-count (cache-hashtable cache)))

(defun (setf get-cached) (value key cache)
  "Add new cached value or update current value associated with
key. Moves new or updated cache element to top of cache list. May
result in least recently used element element being removed."
  (let ((element (gethash key (cache-hashtable cache))))
    (cond ((null element)
           (set-cache-top 
            (setf (gethash key (cache-hashtable cache))
                  (make-cached :key key :value value))
            cache)
           (when (cache-add-notify-fn cache)
             (funcall (cache-add-notify-fn cache) key value cache)))
          (t 
           (move-to-head element cache)
           (setf (cached-value element) value)
           (move-to-head element cache)))
    (when (> (cache-size cache)
             (cache-max-size cache))
      (rm-cache (cache-tail cache) cache))
    value))

(defun remove-cached (key cache)
  "Remove specified element from cache"
  (let ((element (gethash key (cache-hashtable cache))))
    (when element
      (rm-cache element cache)
      element)))

(defun clear-cache (cache)
  "Clear all data from cache."
  (clrhash (cache-hashtable cache))
  (setf 
   (cache-head cache) nil
   (cache-tail cache) nil))

(defun map-cache (fn cache)
  "Iterates over all entries in the cache. For each entry the function
fn is called with two arguments, the cache key and the cached
value. The function can change the cached value with setf of get-cache
or it can remove the cache entry with remove-cached."
  (maphash #'(lambda (k v)
               (funcall fn k (cached-value v)))
           (cache-hashtable cache)))

;;
;; Tests and test helper functions
;;

(defun cached-keys (cache)
  "For testing, quick way to view keys in hashtable."
  (let ((keys))
    (maphash #'(lambda (k v) (push k keys)) (cache-hashtable cache))
    keys))

(defun linked-list-length (cache)
  (labels ((lllen (ll len)
             (if (null ll)
                 len
                 (lllen (cached-next ll) (1+ len)))))
    (lllen (cache-head cache) 0)))

(defun list-vals-not-in-hash (cache)
  "Return items in the mru cache's linked list that are not in the cache."
  (let ((res))
    (labels ((lvnic (ll)
               (when ll
                 (format t "testing: ~s~%" (cached-key ll))
                  (when (null (gethash (cached-key ll) (cache-hashtable cache)))
                    (format t "found uncached: ~s~%" (cached-key ll))
                    (push ll res))
                 (lvnic (cached-next ll)))))
      (lvnic (cache-head cache)))
    res))

(defun find-key-in-list (cache key)
  (labels ((fkil (ll)
             (cond ((null ll)
                    nil)
                   ((equal (cached-key ll) key)
                    ll)
                   (t (fkil (cached-next ll))))))
    (fkil (cache-head cache))))

(defun hash-keys-not-in-list (cache)
  (let ((res))
    (maphash #'(lambda (k v)
                 (unless (find-key-in-list cache k)
                   (push k res)))
             (cache-hashtable cache))))

(defun test-cache-consistency (cache)
  (format t "Cache size: ~S, linked list length: ~S~%" 
          (cache-size cache) (linked-list-length cache))
  (format t "Keys not in list: ~S~%" (hash-keys-not-in-list cache))
  (format t "List values not in hash: ~S~%" (list-vals-not-in-hash cache)))

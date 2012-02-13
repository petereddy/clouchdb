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

;; The encoder in the cl-json package didn't work the way I needed it
;; to, hence this code which is mostly stolen from that package.

(in-package :clouchdb)

(defparameter *json-lisp-escaped-chars*
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defun lisp-special-char-to-json (lisp-char)
    (car (rassoc lisp-char *json-lisp-escaped-chars*)))

(defun write-json-chars (s stream)
  (declare (inline lisp-special-char-to-json))
  (loop for ch across s
        for code = (char-code ch)
        for special = (lisp-special-char-to-json ch)
        do
        (cond
          ((and special (not (char= special #\/)))
           (write-char #\\ stream)
           (write-char special stream))
          ((<= code #x1f)
           (format stream "\\u~4,'0x" code))
          (t (write-char ch stream)))))

(defun write-json-string (s stream)
  (write-char #\" stream)
  (write-json-chars s stream)
  (write-char #\" stream))

(defun write-json-number (nr stream)
  (if (integerp nr)
      (format stream "~d" nr)
      (format stream "~f" nr)))

(defun write-json-symbol(symbol stream)
  (cond
    ((null symbol) (write-json-chars "null" stream))
    ((eq 't symbol) (write-json-chars "true" stream))
    (t (write-json-string (as-field-name-string symbol) stream))))

(defun keyword-assocp (e)
  "Return true if element is a list that begins with a keyword. This
  is used to help determine associative list-ness."
  (and (listp e) (keywordp (car e))))

(defun assoclp (e)
  "Return true if expression is, or really looks like, an associative
list. Dead giveaways include cons elements in the list that begin with
a keyword. Returns the element that produced a positive result, or
nil."
  (labels ((improperlistp (list) 
             (and (listp list)
                  (not (listp (cdr list)))))
           (test (list)
             (cond ((or (null list) (not (listp list)))
                    nil)
                   ((keyword-assocp (car list))
                    (car list))
                   ((improperlistp (car list))
                    (car list)))))
    (and (listp e) (test e))))

(defun write-alist (d stream)
  (write-char #\{ stream)
  (loop for e on d 
     do 
       (let ((cons (car e)))
         (cond ((stringp (car cons))
                (write-string (doublequote (car cons)) stream))
               ((symbolp (car cons))
                (write-json-symbol (car cons) stream)))
         (write-char #\: stream)
         (encode (cdr (car e)) stream))
     when (cdr e) do (write-char #\, stream))
  (write-char #\} stream))

(defun write-list (d stream)
  (write-char #\[ stream)
  (loop for e on d 
     do (encode (car e) stream)
     when (cdr e) do (write-char #\, stream))
  (write-char #\] stream))

(defun encode (d stream)
  (cond ((null d)
         (write-string "null" stream))
        ((numberp d)
         (write-json-number d stream))
        ((symbolp d)
         (write-json-symbol d stream))
        ((stringp d)
         (write-json-string d stream))
        ((assoclp d)
         (write-alist d stream))
        ((listp d)
         (write-list d stream))))

(defun document-to-json-stream (doc stream)
  "Encode document to stream with special support for detecting and
handling associative lists."
  (if (null doc)
      (write-string "{}" stream)
      (encode doc stream)))

(defun document-to-json (doc)
  "Encode document to string with special support for detecting and
handling associative lists."
 (with-output-to-string (stream)
   (document-to-json-stream doc stream)))

;;;; test-api.lisp --- CL-MUSTACHE API test

;;; Copyright (C) 2012  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(in-package :cl-user)
(defpackage #:mustache-test-api
  (:use #:cl #:prove))
(in-package :mustache-test-api)

(defclass unreadable-class ()
  ())

(plan 15)

(is-type (mustache:version) 'string
         "(mustache:version) is a version string")

(is-type (mustache:compile-template "string") 'function
         "compile a string template")
(is-type (mustache:compile-template
          (make-pathname
           :directory (pathname-directory
                       #.(or *load-truename* *compile-file-truename*))
           :name "test"
           :type "mustache"))
         'function
         "compile a file template")

(is (with-output-to-string (mustache:*output-stream*)
      (mustache:render "TEMPLATE"))
    "TEMPLATE"
    "render to mustache:*output-stream*")
(is (mustache:render* "TEMPLATE")
    "TEMPLATE"
    "render to string")
(is (with-output-to-string (out)
      (mustache:render "TEMPLATE" nil out))
    "TEMPLATE"
    "render to out stream")

(mustache:define test-mustache-define "TEMPLATE")
(is (with-output-to-string (mustache:*output-stream*)
      (test-mustache-define))
    "TEMPLATE"
    "mustache:define works")

(is (mustache:render* "{{#list}}{{item}}{{/list}}"
                      '((list . (((item . "a"))
                                 ((item . "b"))
                                 ((item . "c"))))))
    "abc"
    "render context from list")

(is (mustache:render* "{{var}}"
                      `((:var . ,(make-array 0 :element-type 'character
                                               :adjustable t
                                               :fill-pointer 0))))
    ""
    "render a string of type '(array character (*))")

(is (mustache:render* "{{var}}"
                      (let ((context (make-hash-table :test #'equal)))
                        (setf (gethash "VAR" context) "test")
                        context))
    "test"
    "use a hash-table as context.")

(is (mustache:render* "{{escape}}" '((escape . "<>&\"'")))
    "&lt;&gt;&amp;&quot;&#39;"
    "escape char")

(is (mustache:render* "{{var}}" '((var . "pass") (var . "fail")))
    "pass"
    "use alist as context, the first match should shadow the rest.")

(let ((unreadable-instance (make-instance 'unreadable-class)))
  (is (mustache:render* "!!{{unknown-type}}!!"
                        `((unknown-type . ,unreadable-instance)))
      (format nil "!!~a!!" (mustache::escape
                            (princ-to-string unreadable-instance)))
      "escape non printable types"))


(is (mustache:render* "{{symbol}}"
                      '((symbol . symbol)))
    "SYMBOL"
    "print symbols")

(is (mustache:render* "{{#list}}{{var}}{{/list}}"
                      '((:var . "foo")
                        (:list . (1 2 3))))
    "foofoofoo"
    "look in parent context for key")

(finalize)

;;; test-api.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

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

(in-package :mustache-test)
(defsuite api-suite (mustache-suite))

(deftest mustache-type (api-suite)
  (assert-true (typep (mustache-type) 'string)))

(deftest mustache-version (api-suite)
  (assert-true (typep (mustache-version) 'string)))

(deftest mustache-compile-rv (api-suite)
  (assert-true (typep (mustache-compile "string") 'function))
  (assert-true (typep (mustache-compile
                       (make-pathname
                        :directory (pathname-directory
                                    #.(or *load-truename* *compile-file-truename*))
                        :name "test"
                        :type "mustache"))
                      'function)))

(deftest render-to-*mustache-output* (api-suite)
  (assert-equal "TEMPLATE"
      (with-output-to-string (*mustache-output*)
        (mustache-render "TEMPLATE"))))

(deftest render-to-string (api-suite)
  (assert-equal "TEMPLATE"
      (mustache-render-to-string "TEMPLATE")))

(deftest render-to-stream (api-suite)
  (assert-equal "TEMPLATE"
      (with-output-to-string (out)
        (mustache-render-to-stream out "TEMPLATE"))))

(deftest defmustache (api-suite)
  (defmustache test-defmustache "TEMPLATE")
  (assert-equal "TEMPLATE"
      (with-output-to-string (*mustache-output*)
        (test-defmustache))))

(deftest render-context-from-list (api-suite)
  (assert-equal "abc"
      (mustache-render-to-string "{{#list}}{{item}}{{/list}}"
                                 (mustache-context
                                  :data '((list . (((item . "a"))
                                                   ((item . "b"))
                                                   ((item . "c")))))))))

(deftest render-character-string (api-suite)
  (assert-equal ""
      (mustache-render-to-string "{{var}}"
                                 `((:var . ,(make-array 0 :element-type 'character
                                                          :adjustable t
                                                          :fill-pointer 0))))))

(deftest hash-table-context (api-suite)
  (assert-equal "test"
      (mustache-render-to-string "{{var}}"
                                 (let ((context (make-hash-table :test #'equal)))
                                   (setf (gethash "VAR" context) "test")
                                   context))))

(deftest escape-char (api-suite)
  (assert-equal "&lt;&gt;&amp;&quot;&apos;"
      (mustache-render-to-string "{{escape}}"
                                 (mustache-context
                                  :data '((escape . "<>&\"'"))))))

(deftest alist-context (api-suite)
  (assert-equal "pass"
      (mustache-render-to-string "{{var}}"
                                 (mustache-context
                                  :data '((var . "pass")
                                          (var . "fail"))))
    "use alist as context, the first match should shadow the rest."))

;;; test-api.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

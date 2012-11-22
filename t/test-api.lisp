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

(deftest api
    (progn
      (is-type (mustache-type) 'string
               "mustache-type is a version string")
      (is-type (mustache-version) 'string
               "mustache-version is a version string")

      (is-type (mustache-compile "string") 'function
               "compile a string template")
      (is-type (mustache-compile
                (make-pathname
                 :directory (pathname-directory
                             #.(or *load-truename* *compile-file-truename*))
                 :name "test-api"
                 :type "lisp"))
               'function
               "compile a file template")

      (is (with-output-to-string (*mustache-output*)
            (mustache-render "TEMPLATE"))
          "TEMPLATE"
          "render to *mustache-output*")
      (is (mustache-render-to-string "TEMPLATE")
          "TEMPLATE"
          "render to string")
      (is (with-output-to-string (out)
            (mustache-render-to-stream out "TEMPLATE"))
          "TEMPLATE"
          "render to out stream")

      (defmustache test-defmustache "TEMPLATE")
      (is (with-output-to-string (*mustache-output*)
            (test-defmustache))
          "TEMPLATE"
          "defmustache works")

      (is (mustache-render-to-string "{{#list}}{{item}}{{/list}}"
                                     (mustache-context
                                      :data '((list . (((item . "a"))
                                                       ((item . "b"))
                                                       ((item . "c")))))))
          "abc"
          "render context from liste")

      (is (mustache-render-to-string "{{escape}}"
                                     (mustache-context
                                      :data '((escape . "<>&\"'"))))
          "&lt;&gt;&amp;&quot;&apos;"
          "escape char")))

;;; test-api.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

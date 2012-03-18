;;;; test.lisp --- Test against the specs

;;; Copyright (C) 2011  Kan-Ru Chen

;;; Author: Kan-Ru Chen <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;;; Commentary:

;;; Inspired by and some codes copied directly from
;;; http://msnyder.info/posts/2011/12/common-lisp-mustache/

;;;; Code:

(defpackage :mustache-test
  (:use :cl :mustache)
  (:import-from #:cl-fad
                #:walk-directory)
  (:import-from #:cl-markup
                #:markup
                #:*output-stream*)
  (:export #:run-test
           #:start-test-server
           #:stop-test-server
           #:*spec-directory*))

(in-package :mustache-test)

(defparameter *spec-directory*
  (make-pathname
   :directory
   (append (pathname-directory
            #.(or *load-truename* *compile-file-truename*))
           '("spec" "specs"))))

(defvar *test-acceptor* nil)

(defun utf8-json-decode (pathname)
  (with-open-file (stream pathname
                          :direction :input)
    (let ((json:*json-array-type* 'vector))
      (json:decode-json-from-source stream))))

(defun json-file-p (pathname)
  (string= "json" (pathname-type pathname)))

(defun all-specs ()
  (let (specs)
    (walk-directory *spec-directory*
                    (lambda (file)
                      (push (utf8-json-decode file) specs))
                    :test #'json-file-p)
    specs))

(defun compile-lambda (data)
  (let* ((lambda (cdr (assoc :lambda data)))
         (lisp (cdr (assoc :lisp lambda))))
    (if lisp
        (append data (acons :lambda (eval (read-from-string lisp)) nil))
        data)))

(defun test-result (expected template data partials)
  (let ((result (ignore-errors (mustache-render-to-string template (mustache-context :data (compile-lambda data) :partials partials)))))
    (if result
        (list (if (string= expected result)
                  :test-passed
                  :test-failure)
              result)
        (list :unexpected-test-failure ""))))

(defun test-specs (specs)
  (loop for spec in specs
        append (loop for test across (cdr (assoc :tests spec))
                     for name     = (cdr (assoc :name test))
                     for template = (cdr (assoc :template test))
                     for data     = (cdr (assoc :data test))
                     for expected = (cdr (assoc :expected test))
                     for desc     = (cdr (assoc :desc test))
                     for partials = (cdr (assoc :partials test))
                     collect (list name desc (test-result expected template data partials) template expected data partials))))

(defun start-test-server (&optional (port 0))
  (setf *test-acceptor* (toot:start-server :port port :handler 'report-handler))
  (format nil "http://localhost:~d/" (toot:port *test-acceptor*)))

(defun stop-test-server ()
  (toot:stop-acceptor *test-acceptor*))

(defun html-format-test-results (s results)
  (let ((*output-stream* s))
    (write-string "<!doctype html>" *output-stream*)
    (markup
     (:html
       (:head
        (:style :type "text/css"
                ".TEST-PASSED { background-color: #0f0; }"
                ".TEST-FAILURE { background-color: #f00; }"
                ".UNEXPECTED-TEST-FAILURE { background-color: #ff0; }"))
       (:body
        (:table
         (loop
           for (name description (result r) template expected data partials) in results
           do (markup (:tr
                       (:td name)
                       (:td description)
                       (:td :class result result)))
           when (not (eq result :test-passed))
             do (markup
                 (:tr
                  (:td (:pre template))
                  (:td (:pre expected))
                  (:td (:pre (or r ""))))))))))))

(defun text-format-test-results (s results)
  (loop for (name desc (result _) template expected data) in results
        do (case result
             (:test-passed (format s "."))
             (t (format s "~&~A [~A]~%`--> ~A~&" name result desc)))))

(defun run-test ()
  (let ((results (test-specs (all-specs))))
    (text-format-test-results t results)))

(defun report-handler (request)
  (let ((s (toot:send-headers request))
        (results (test-specs (all-specs))))
    (html-format-test-results s results)))

;;; test.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

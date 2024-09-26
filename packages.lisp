;;;; packages.lisp --- Packages of cl-mustache

;;; Copyright (C) 2012  Kan-Ru Chen

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

;;;

;;;; Code:

(in-package #:cl-user)

(defpackage #:mustache
  (:use #:cl)
  (:export #:*load-path*
           #:*default-pathname-type*
           ;; new
           #:*output-stream*
           #:*context*
           #:*escape-tokens*
           #:version
           #:make-context
           #:compile-template
           #:render
           #:render*
           #:define
           ;; old
           #:*mustache-output*
           #:mustache-type
           #:mustache-version
           #:mustache-context
           #:mustache-compile
           #:mustache-render
           #:mustache-render-to-string
           #:mustache-render-to-stream
           #:partial-cant-be-found
           #:defmustache))


;;; packages.lisp ends here

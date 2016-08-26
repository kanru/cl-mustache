;;;; compat-api-v1.lisp --- APIv1 compatibility package

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

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

;;;; Code:

(in-package :mustache)

(defvar *mustache-output* (make-synonym-stream '*output-stream*)
  "Deprecated in favor of MUSTACHE:*OUTPUT-STREAM* since version 0.10.0")

(define-condition deprecation-warning (style-warning)
  ((name :initarg :name :reader deprecated-name)
   (replacements :initarg :replacement :reader deprecated-name-replacement)
   (since :initarg :since :reader deprecated-since))
  (:report (lambda (condition stream)
             (format stream "~S is deprecated since CL-MUSTACHE version ~A. ~
                             Use ~S instead."
                     (deprecated-name condition)
                     (deprecated-since condition)
                     (deprecated-name-replacement condition)))))

(defmacro make-obsolete (obsolete-name current-name when)
  `(defun ,obsolete-name (&rest args)
     ,(documentation current-name 'function)
     (warn 'deprecation-warning
           :name ',obsolete-name :replacement ',current-name :since ,when)
     (apply ',current-name args)))

(make-obsolete mustache-type    version "0.10.0")
(make-obsolete mustache-version version "0.10.0")
(make-obsolete mustache-context make-context "0.10.0")
(make-obsolete mustache-compile compile-template "0.10.0")
(make-obsolete mustache-render  render "0.10.0")
(make-obsolete mustache-render-to-string render* "0.10.0")

(defun mustache-render-to-stream (stream template &optional context)
  "Render TEMPLATE with optional CONTEXT to STREAM."
  (warn 'deprecation-warning
        :name 'mustache-render-to-stream :replacement 'render :since "0.10.0")
  (render template context stream))

(defmacro defmustache (name template)
  #.(documentation 'define 'function)
  (warn 'deprecation-warning
        :name 'defmustache :replacement 'render :since "0.10.0")
  `(define ,name ,template))

;;; compat-api-v1.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

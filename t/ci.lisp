;;;; ci.lisp --- CI Script

;;; Copyright (C) 2013  Kan-Ru Chen (陳侃如)

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
(defpackage :mustache-test-ci
  (:use :cl))
(in-package :mustache-test-ci)

(defun exit (code)
  (let ((exit (find-symbol #.(string :exit) :sb-ext))
        (quit (find-symbol #.(string :quit) :sb-ext)))
    (cond
      (exit (funcall exit :code code))
      (quit (funcall quit :unix-status code))
      (t (error "No exit or quit function")))))

(ql:quickload :prove)
(ql:quickload :cl-mustache)

(unless (and (prove:run #P"t/test-api")
             (prove:run #P"t/test-spec"))
  (exit -1))

;;; ci.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

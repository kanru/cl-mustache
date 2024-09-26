;;;; gen-test-spec.lisp --- Test against the specs

;;; Copyright (C) 2011, 2012  Kan-Ru Chen

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

;;;; Code:

(in-package :cl-user)
(require :alexandria)
(require :cl-fad)
(require :cl-json)

(defparameter *spec-directory*
  (make-pathname
   :directory
   (append (pathname-directory
            #.(or *load-truename* *compile-file-truename*))
           '("spec" "specs"))))

(defun utf8-json-decode (pathname)
  (with-open-file (stream pathname
                          :direction :input)
    (let ((json:*json-array-type* 'vector))
      (json:decode-json-from-source stream))))

(defun json-file-p (pathname)
  (string= "json" (pathname-type pathname)))

(defun all-specs ()
  (uiop:while-collecting (collect-spec)
    (uiop:collect-sub*directories
     *spec-directory*
     (constantly t)
     (complement #'uiop:hidden-pathname-p)
     (lambda (dir)
       (mapc (lambda (file)
               (when (json-file-p file)
                 (let* ((decoded (utf8-json-decode file))
                        (relative-filename (uiop:enough-pathname file *spec-directory*))
                        (spec (list* (cons :filename (namestring relative-filename))
                                     decoded)))
                   (collect-spec spec))))
             (uiop:directory-files dir))))))


(defclass lambda-code ()
  ((code :initarg :code
         :type string
         :reader code))
  (:documentation "We need to wrap code into this class to write it to the file the same way as specified in the spec.

I've tried to read lisp code from the spec into the list structure and to serialize it back, but was not able to make it work propertly with backquoting."))


(defmethod print-object ((obj lambda-code) stream)
  (write-string ","
                stream)
  (write-string (code obj)
                stream))


(defclass backquoted-data ()
  ((data :initarg :data
         :reader data))
  (:documentation "We need to wrap data into this class to write as backquouted form.

I've tried to read lisp code from the spec into the list structure and to serialize it back, but was not able to make it work propertly with backquoting."))


(defmethod print-object ((obj backquoted-data) stream)
  (write-string "`"
                stream)
  (write (data obj)
         :stream stream))


(defclass comment ()
  ((message :initarg :message
            :type string
            :reader message)
   (arguments :initarg :arguments
              :type list
              :initarg nil
              :reader arguments)))


(defun make-comment (message &rest arguments)
  (make-instance 'comment
                 :message message
                 :arguments arguments))


(defmethod print-object ((obj comment) stream)
  (write-string ";; "
                stream)
  (apply #'format stream (message obj)
         (arguments obj)))


(defun compile-code-block-if-needed (value)
  (cond
    ((and (listp value)
          (consp (first value))
          (equal (alexandria:assoc-value value
                                         :----TAG----)
                 "code"))
     (let ((code (alexandria:assoc-value value
                                         :lisp)))
       (unless code
         (error "Unable to find LISP code for the test."))
       (make-instance 'lambda-code
                      :code code)))
    (t
     (compile-code-blocks value))))


(defun compile-code-blocks (data)
  (typecase data
    (list
     (loop for (key . value) in data
           for new-value = (compile-code-block-if-needed value)
           collect (cons key new-value)
           ;; finally (return `(list ,@results))
           ))
    (array
     (map (type-of data)
          #'compile-code-blocks
          data))
    (t
     data)))


(defun make-test-form (test)
  (let ((name (alexandria:assoc-value test :name))
        (template (alexandria:assoc-value test :template))
        (data (make-instance 'backquoted-data
                             :data (compile-code-blocks
                                    (alexandria:assoc-value test :data))))
        (expected (alexandria:assoc-value test :expected))
        (desc (alexandria:assoc-value test :desc))
        (partials (alexandria:assoc-value test :partials)))
    `(is (mustache:render*
          ,template
          (mustache:make-context
           :data ,data
           :partials ',partials))
         ,expected
         (format nil "~A :: ~A" ,name ,desc))))


;; Generate test file

(defun update-spec-tests ()
  (alexandria:with-output-to-file (stream (asdf:system-relative-pathname
                                           :cl-mustache
                                           (make-pathname :directory '(:relative "t")
                                                          :name "test-spec"
                                                          :type "lisp"))
                                          :if-exists :supersede)
    (let ((*print-case* :downcase))
      (format stream ";; WARNING! This file was generated by gen-test-spec.lisp script.
;;          Don't edit it manually!
;;          To update the file, clone spec repository:
;;
;;          git clone https://github.com/mustache/spec t/spec
;;
;;          Then do this in the REPL:
;;
;;          CL-USER> (ql:quickload '(cl-json cl-fad alexandria))
;;          CL-USER> (load \"t/gen-test-spec.lisp\")
;;          CL-USER> (cl-user::update-spec-tests)~2%")
      (pprint '(defpackage #:mustache-test-spec
                (:use #:cl #:prove))
              stream)
      (pprint '(in-package :mustache-test-spec)
              stream)
      (terpri stream)
      
      (let ((pieces)
            (num-tests 0))
        
        (loop for spec in (all-specs)
              do (push (make-comment "Spec filename: ~A~%"
                                     (alexandria:assoc-value spec :filename))
                       pieces)
                 (loop for test across (alexandria:assoc-value spec :tests)
                       do (push (make-test-form test)
                                pieces)
                          (incf num-tests)))
        
        (pprint `(plan ,num-tests)
                stream)
        (terpri stream)

        (loop for piece in (reverse pieces)
              do (pprint piece stream)
                 (terpri stream)))
      
      (pprint '(finalize)
              stream))))

;;; gen-test-spec.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

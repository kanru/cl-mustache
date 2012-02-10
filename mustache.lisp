;;;; mustache.lisp --- Mustache Template Renderer

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

;;; Mustache is a kind of logic-less template formats.
;;; See also: http://mustache.github.com/

;;;; TODO:
;;;
;;; * Optimize lambda sections
;;; * Optimize compiled renderer

;;;; Code:

(in-package :mustache)

;;; Delimiter

(defparameter *default-open-delimiter* "{{")
(defparameter *default-close-delimiter* "}}")
(defparameter *default-triple-open-delimiter* "{{{")
(defparameter *default-triple-close-delimiter* "}}}")

(defvar *open-delimiter*)
(defvar *close-delimiter*)
(defvar *triple-open-delimiter* *default-triple-open-delimiter*)
(defvar *triple-close-delimiter* *default-triple-close-delimiter*)

(defun change-delimiter (text)
  (let* ((text (string-trim '(#\Space #\Tab #\=) text))
         (left-end (position #\Space text))
         (right-start (position #\Space text :from-end t))
         (right-end (position #\= text :from-end t)))
    (setf *open-delimiter* (subseq text 0 left-end))
    (setf *close-delimiter* (subseq text (1+ right-start) right-end))))

;;; Parser

(deftype space-char () '(member #\Space #\Tab))
(deftype newline-char () '(member #\Newline #\Return))
(deftype text-char () '(not (or space-char newline-char)))

(defclass token () ())
(defclass noop () ())
(defclass beginning-of-line (token) ())

(defclass text (token)
  ((text :initarg :text :accessor text)))
(defclass whitespace (text) ())
(defclass newline (text)
  ((text :initform (coerce '(#\Newline) 'string))))
(defclass crlf-newline (newline)
  ((text :initform (coerce '(#\Return #\Newline) 'string))))

(defclass tag (token)
  ((text :initarg :text :accessor text)
   (escape :initarg :escape :initform nil :accessor escapep)
   (standalone :initform nil :accessor standalone)
   (indent :initarg :indent :initform nil :accessor indent)
   (trail :initarg :trail :initform nil :accessor trail)))
(defclass can-standalone-tag (tag) ())
(defclass non-standalone-tag (tag) ())

(defclass normal-tag (non-standalone-tag) ())
(defclass implicit-iterator-tag (non-standalone-tag) ())
(defclass ampersand-tag (non-standalone-tag) ())
(defclass delimiter-tag (can-standalone-tag noop) ())
(defclass comment-tag (can-standalone-tag noop) ())
(defclass partial-tag (can-standalone-tag) ())

(defclass section-start-tag (can-standalone-tag)
  ((falsey :initarg :falsey :initform nil :accessor falsey)
   (end :initarg :end :initform nil :accessor end)
   (open-delimiter :initarg :open-delimiter :initform nil :accessor open-delimiter)
   (close-delimiter :initarg :close-delimiter :initform nil :accessor close-delimiter)))
(defclass section-end-tag (can-standalone-tag)
  ((start :initarg :start :initform nil :accessor start)))
(defclass section-tag (can-standalone-tag)
  ((tokens :initarg :tokens :accessor tokens)
   (falsey :initarg :falsey :initform nil :accessor falsey)
   (start :initarg :start :initform nil :accessor start)
   (end :initarg :end :initform nil :accessor end)
   (open-delimiter :initarg :open-delimiter :initform nil :accessor open-delimiter)
   (close-delimiter :initarg :close-delimiter :initform nil :accessor close-delimiter)))

(defun make-tag (&key text escape start end)
  (let ((funchar (char text 0))
        (tag-text (string-trim '(#\Space #\Tab) (subseq text 1)))
        (text (string-trim '(#\Space #\Tab) text)))
   (case funchar
     (#\& (make-instance 'ampersand-tag :text tag-text))
     (#\# (make-instance 'section-start-tag :text tag-text :end end
                                            :open-delimiter *open-delimiter*
                                            :close-delimiter *close-delimiter*))
     (#\^ (make-instance 'section-start-tag :text tag-text :end end :falsey t))
     (#\/ (make-instance 'section-end-tag :text tag-text :start start))
     (#\! (make-instance 'comment-tag :text ""))
     (#\= (prog1
              (make-instance 'delimiter-tag :text tag-text)
            (change-delimiter tag-text)))
     (#\> (make-instance 'partial-tag :text tag-text))
     (#\. (make-instance 'implicit-iterator-tag :text tag-text))
     (t (make-instance 'normal-tag :text text :escape escape)))))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (text object) stream)))

;;; Lexer

;; Invariant token
(defvar beginning-of-line (make-instance 'beginning-of-line))
(defvar newline (make-instance 'newline))
(defvar crlf-newline (make-instance 'crlf-newline))

(defun string-match (pattern string &optional (start 0))
  (let ((end2 (+ start (length pattern)))
        (len (length string)))
    (and (>= len end2)
         (string= pattern string :start2 start :end2 end2))))

(defun text-char-p (char)
  (typep char 'text-char))

(defun space-char-p (char)
  (typep char 'space-char))

(defun newline-char-p (char)
  (typep char 'newline-char))

(defvar crlf (coerce '(#\Return #\Newline) 'string))

(defun read-text (type string &optional (start 0) (end (length string)))
  (loop for idx from start below end
        while (case type
                (text (text-char-p (char string idx)))
                (whitespace (space-char-p (char string idx))))
        until (string-match *open-delimiter* string idx)
        finally (return (values (make-instance type :text (subseq string start idx))
                                idx))))

(defun read-newline (string &optional (start 0) (end (length string)))
  (declare (ignore end))
  (cond
    ((string-match crlf string start)
     (values crlf-newline
             (+ 2 start)))
    ((newline-char-p (char string start))
     (values newline
             (1+ start)))))

(defun read-tag (string &optional triple (start 0) (end (length string)))
  (let ((before-tag start)
        (tag-open (if triple *triple-open-delimiter* *open-delimiter*))
        (tag-close (if triple *triple-close-delimiter* *close-delimiter*)))
    (when (string-match tag-open string start)
      (incf start (length tag-open))
      (loop for idx from start below end
            until (string-match tag-close string idx)
            finally (let ((endpos (+ idx (length tag-close))))
                      (return (values (make-tag :text (subseq string start idx)
                                                :escape (not triple)
                                                :start before-tag
                                                :end endpos)
                                      endpos)))))))

(defun read-token (string &optional (start 0) (end (length string)))
  (let ((char (char string start)))
    (cond
      ((space-char-p char)
       (read-text 'whitespace string start end))
      ((newline-char-p char)
       (read-newline string start end))
      ((string-match *triple-open-delimiter* string start)
       (read-tag string t start end))
      ((string-match *open-delimiter* string start)
       (read-tag string nil start end))
      (t
       (read-text 'text string start end)))))

(defun scan (string &optional (start 0) (end (length string)))
  (let ((idx start)
        (*open-delimiter* *default-open-delimiter*)
        (*close-delimiter* *default-close-delimiter*))
    (loop while (> end idx)
          with token
          when (zerop idx)
            collect beginning-of-line
          do (multiple-value-setq (token idx)
               (read-token string idx))
          collect token
          when (and (< idx end)
                    (typep token 'newline))
            collect beginning-of-line)))

;;; Parser

(deftype text-token ()
  '(not (or beginning-of-line can-standalone-tag newline whitespace)))

(defun newlinep (token)
  (typep token 'newline))

(defun tagp (token)
  (typep token 'tag))

(defun collect-line (tokens)
  (loop for start = 0 then (1+ finish)
        for finish = (position-if #'newlinep tokens :start start)
        when (subseq tokens start (and finish (1+ finish)))
        collect it
        until (null finish)))

(defun standalone-p (tokens)
  (when (eq (car tokens) beginning-of-line)
    (loop for token in tokens
          count (typep token 'can-standalone-tag) into tags
          count (typep token 'text-token) into texts
          finally (return (and (= 1 tags)
                               (= 0 texts))))))

(defun make-standalone-tag (tokens)
  (let* ((pos (position-if #'tagp tokens))
         (tag (elt tokens pos)))
    (setf (indent tag) (subseq tokens 0 pos))
    (setf (trail tag) (subseq tokens (1+ pos)))
    (setf (standalone tag) t)
    tag))

(defun trim-standalone (tokens)
  (loop for line in (collect-line tokens)
        append (if (standalone-p line)
                   (list (make-standalone-tag line))
                   line)))

(defun tag-match (tag1 tag2)
  (string-equal (text tag1) (text tag2)))

(defun make-section-tag (start-tag end-tag tokens)
  (make-instance 'section-tag
                 :tokens tokens
                 :text (text start-tag)
                 :falsey (falsey start-tag)
                 :start (end start-tag)
                 :end (start end-tag)
                 :open-delimiter (open-delimiter start-tag)
                 :close-delimiter (close-delimiter start-tag)))

(defun push-group (acc)
  (cons nil acc))

(defun push-token (token acc)
  (cons (cons token (car acc)) (cdr acc)))

(defun pop-group (acc)
  (cdr acc))

(defun top-group (acc)
  (reverse (car acc)))

(defun push-section-tag (start-tag end-tag acc)
  (push-token (make-section-tag start-tag end-tag (top-group acc))
              (pop-group acc)))

(defun group-sections (tokens &optional sections acc)
  (if (not tokens)
      (top-group acc)
      (let ((token (car tokens))
            (rest (cdr tokens))
            (start-tag (car sections)))
        (typecase token
          (section-start-tag
           (group-sections rest (cons token sections) (push-group acc)))
          (section-end-tag
           (when (tag-match token start-tag)
             (group-sections rest (cdr sections)
                             (push-section-tag start-tag token acc))))
          (otherwise
           (group-sections rest sections (push-token token acc)))))))

(defun textp (token)
  (typep token 'text))

(defun fold-text (tokens)
  (loop for start = 0 then next
        for finish = (position-if (complement #'textp) tokens :start start)
        for next = (and finish (position-if #'textp tokens :start finish))
        for texts = (subseq tokens start finish)
        when texts
          collect (make-instance 'text :text
                                 (format nil "~{~a~}" (mapcar #'text texts)))
        when (and finish
                  (subseq tokens finish next))
          append it
        while next))

(defun parse (template)
  (group-sections (fold-text (trim-standalone (scan template)))))

;;; Context

(defclass context ()
  ((data :initarg :data
         :initform nil
         :accessor data)
   (indent :initarg :indent
           :initform nil
           :accessor indent)
   (partials :initarg :partials
             :initform nil
             :accessor partials)
   (next :initarg :next
         :initform nil
         :accessor next)))

(defun parse-key (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\. string :start start)
        collect (intern (string-upcase (subseq string start finish)) :keyword)
        until (null finish)))

(defun save-hash-table (source)
  (typecase source
    (string source)
    (null nil)
    (vector
     (and (plusp (length source))
          (map 'vector #'save-hash-table source)))
    (list
     (let ((table (make-hash-table)))
       (dolist (cons source)
         (setf (gethash (car cons) table) (save-hash-table (cdr cons))))
       table))
    (otherwise
     source)))

(defun mustache-context (&key data partials)
  "Create mustache context from alist DATA."
  (make-instance 'context :data (save-hash-table data)
                          :partials (save-hash-table partials)))

(defun make-context (&optional data context)
  (let ((ctx (make-instance 'context)))
    (if context
        (progn
          (setf (data ctx) data)
          (setf (indent ctx) (indent context))
          (setf (partials ctx) (partials context))
          (setf (next ctx) context)))
    ctx))

(defgeneric context-get (key context)
  (:documentation "Get data from CONTEXT by KEY."))

(defmethod context-get ((key string) context)
  (context-get (parse-key key) context))

(defmethod context-get ((key list) context)
  (multiple-value-bind (data find)
      (context-get (car key) context)
    (if (cdr key)
        (context-get (cdr key) data)
        (values data find))))

(defmethod context-get ((key symbol) (context hash-table))
  (gethash key context))

(defmethod context-get ((key symbol) (context context))
  (multiple-value-bind (data find)
      (context-get key (data context))
    (if find
        (values data find)
        (when (next context)
          (context-get key (next context))))))

(defmethod context-get (key (context null))
  (declare (ignore key))
  (values))

(defmethod (setf context-get) (value key context)
  (setf (gethash key (data context)) value))

;;; Partials

(defvar *load-path* (list *default-pathname-defaults*))
(defvar *default-pathname-type* "mustache")

(defun locate-file (filename)
  (labels ((filename (path filename)
           (merge-pathnames
            path (make-pathname
                  :type *default-pathname-type*
                  :defaults (pathname-as-file filename))))
           (dir-file-exists-p (path)
             (file-exists-p (filename path filename))))
      (some #'dir-file-exists-p *load-path*)))

(defun read-partial (filename &optional context)
  (let ((from-context (context-get filename (partials context))))
    (if from-context
        from-context
        (let ((pathname (locate-file filename)))
          (when pathname
            (with-open-file (stream pathname)
              (let ((buffer (make-string (file-length stream))))
                (read-sequence buffer stream)
                buffer)))))))

;;; Rendering Utils

(defparameter *char-to-escapes* "<>&\"'")

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (string)
  (flet ((needs-escape-p (char) (find char *char-to-escapes*)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p string :start start)
            do (write-sequence string out :start start :end pos)
            when pos
              do (write-sequence (escape-char (char string pos)) out)
            while pos))))

(defmethod escapep ((object ampersand-tag)) nil)
(defmethod (setf escapep) (new-value (object ampersand-tag)) nil)

(defvar *mustache-output* *standard-output*)

(defgeneric print-data (data escapep &optional context))

(defmethod print-data ((data string) escapep &optional context)
  (declare (ignore context))
  (princ (if escapep (escape data) data) *mustache-output*))

(defmethod print-data ((data function) escapep &optional context)
  (let* ((value (format nil "~a" (funcall data)))
         (fun (mustache-compile value))
         (output (with-output-to-string (*mustache-output*)
                   (funcall fun context))))
    (princ (if escapep (escape output) output) *mustache-output*)))

(defmethod print-data (token escapep &optional context)
  (declare (ignore escapep context))
  (princ token *mustache-output*))

(defun print-indent (&optional context)
  (when (and context
             (indent context))
    (funcall (car (indent context)) nil)))

(defmethod call-lambda (lambda text &optional context)
  (let* ((value (format nil "~a" (funcall lambda text)))
         (fun (mustache-compile value))
         (output (with-output-to-string (*mustache-output*)
                   (funcall fun context))))
    (princ output *mustache-output*)))

;;; Compiler

(defgeneric emit-token (token))

(defmethod emit-token ((token text))
  `(print-data ,(text token) nil nil))

(defmethod emit-token ((token tag))
  `(multiple-value-bind (dat find)
       (context-get ,(text token) context)
     (when find (print-data dat ,(escapep token) context))))

(defmethod emit-token ((token partial-tag))
  `(let ((fun (mustache-compile
               (or (read-partial ,(text token) context) ""))))
     (push (lambda (&optional context)
             (declare (ignorable context))
             ,@(emit-tokens (indent token)))
           (indent context))
     (funcall fun context)
     (pop (indent context))))

(defmethod emit-token ((token section-tag))
  `(multiple-value-bind (ctx find)
       (context-get ,(text token) context)
     (when (or find ,(falsey token))
       (flet ((fun (&optional context)
                (declare (ignorable context))
                ,@(emit-tokens (tokens token))))
         ,(if (falsey token)
              `(when (null ctx)
                 (fun context))
              `(typecase ctx
                 (hash-table
                  (fun (make-context ctx context)))
                 (null)
                 (function
                  (let ((*default-open-delimiter* ,(open-delimiter token))
                        (*default-close-delimiter* ,(close-delimiter token)))
                    (call-lambda ctx (subseq template ,(start token) ,(end token)) context)))
                 (vector
                  (loop for ctx across ctx
                        do (fun (make-context ctx context))))
                 (t
                  (fun context))))))))

(defmethod emit-token ((token implicit-iterator-tag))
  `(print-data (data context) ,(escapep token) context))

(defmethod emit-token ((token beginning-of-line))
  (declare (ignore token))
  `(print-indent context))

;; noop tokens
(defmethod emit-token ((token noop))
  `(values))

(defun emit-tokens (tokens)
  (loop for token in tokens
        collect (emit-token token)))

(defun emit-body (tokens template)
  `((let ((context (if (listp context)
                       (mustache-context :data context)
                       context))
          (template ,template))
      (declare (ignorable context template))
      (with-standard-io-syntax
        ,@(emit-tokens tokens)))))

;;; Interfaces

(defun mustache-type ()
  "Mustache spec v1.1.2+λ")

(defun mustache-version ()
  "CL-MUSTACHE v0.9.0")

(defgeneric mustache-compile (template)
  (:documentation "Return a compiled rendering function."))

(defmethod mustache-compile ((template string))
  (compile nil `(lambda (&optional context) ,@(emit-body (parse template) template))))

(defmethod mustache-compile ((template pathname))
  (with-open-file (stream template :if-does-not-exist nil)
    (when stream
      (let ((buffer (make-string (file-length stream))))
        (read-sequence buffer stream)
        (mustache-compile buffer)))))

(defgeneric mustache-render (template &optional context)
  (:documentation "Render TEMPLATE with optional CONTEXT to *mustache-output*"))

(defmethod mustache-render (template &optional context)
  (let ((fun (mustache-compile template)))
    (when fun
      (funcall fun context))))

(defun mustache-render-to-string (template &optional context)
  "Render TEMPLATE with optional CONTEXT to string."
  (with-output-to-string (*mustache-output*)
    (mustache-render template context)))

(defun mustache-render-to-stream (stream template &optional context)
  "Render TEMPLATE with optional CONTEXT to STREAM."
  (let ((*mustache-output* stream))
    (mustache-render template context)))

(defmacro defmustache (name template)
  "Define a named renderer of string TEMPLATE."
  `(defun ,name (&optional context) ,@(emit-body (parse template) template)))

;;; mustache.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

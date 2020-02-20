;;;; mustache.lisp --- Mustache Template Renderer

;;; Copyright (C) 2012, 2013  Kan-Ru Chen

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
;;; * Better error reporting

;;;; Code:

(in-package :mustache)

;;; Types

(deftype offset ()
  "File or string offset."
  '(unsigned-byte 32))

(deftype space-char ()
  '(member #\Space #\Tab))

(deftype newline-char ()
  '(member #\Linefeed #\Return))

(deftype text-char ()
  '(and character (not (or space-char newline-char))))

(defun space-char-p (char)
  (declare (inline))
  (typep char 'space-char))

(defun newline-char-p (char)
  (declare (inline))
  (typep char 'newline-char))

(defun text-char-p (char)
  (declare (inline))
  (typep char 'text-char))

(defclass token () ())

(defclass beginning-of-line (token) ())

(defclass text (token)
  ((%text :type string
          :initarg :text
          :accessor text)))

(defclass whitespace (text) ())

(defclass newline (text)
  ((%text :initform #.(coerce '(#\Linefeed) 'string))))

(defvar crlf (coerce '(#\Return #\Linefeed) 'string))

(defclass crlf-newline (newline)
  ((%text :initform crlf)))

(defclass tag (token)
  ((%text :type string
          :initarg :text
          :accessor text)
   (%escapep :type boolean
             :initarg :escape
             :initform t
             :reader escapep)
   (%indent :type list
            :initarg :indent
            :initform ()
            :accessor indent)))
(defclass can-standalone-tag (tag) ())
(defclass non-standalone-tag (tag) ())

(defclass normal-tag (non-standalone-tag) ())

(defclass ampersand-tag (non-standalone-tag)
  ((%escapep :initform nil)))

(defclass delimiter-tag (can-standalone-tag) ())

(defclass comment-tag (can-standalone-tag) ())

(defclass partial-tag (can-standalone-tag) ())

(defclass section-start-tag (can-standalone-tag)
  ((%falsey :type boolean
            :initarg :falsey
            :initform nil
            :accessor falsey)
   (%end :type offset
         :initarg :end
         :initform 0
         :accessor end)
   (%open-delimiter :type string
                    :initarg :open-delimiter
                    :initform ""
                    :accessor open-delimiter)
   (%close-delimiter :type string
                     :initarg :close-delimiter
                     :initform ""
                     :accessor close-delimiter)))

(defclass section-end-tag (can-standalone-tag)
  ((%start :type offset
           :initarg :start
           :initform 0
           :accessor start)))

(defclass section-tag (section-start-tag section-end-tag)
  ((%tokens :type list
            :initarg :tokens
            :accessor tokens)))

;;; Delimiter

(defparameter *default-open-delimiter* "{{")
(defparameter *default-close-delimiter* "}}")
(defparameter *default-triple-open-delimiter* "{{{")
(defparameter *default-triple-close-delimiter* "}}}")

(defvar *open-delimiter* *default-open-delimiter*)
(defvar *close-delimiter* *default-close-delimiter*)
(defvar *triple-open-delimiter* *default-triple-open-delimiter*)
(defvar *triple-close-delimiter* *default-triple-close-delimiter*)

(defun change-delimiter (text)
  "Change the mustache tag delimiter according to TEXT.
The syntax grammar is:
  delimiter-tag = left-d 1*space right-d
  left-d        = *ALPHANUM
  right-d       = *ALPHANUM
  space         = #\\Space #\\Tab"
  (declare (type string text))
  (let* ((left-edge (position #\Space text))
         (right-edge (position #\Space text :from-end t)))
    (unless (and left-edge right-edge
                 (every #'space-char-p
                        (subseq text left-edge right-edge)))
      (error "Invalid delimiter tag ~a" text))
    (setf *open-delimiter* (subseq text 0 left-edge)
          *close-delimiter* (subseq text (1+ right-edge)))))

;;; Parser

(defvar *mustache-tag-table* (make-hash-table))

(defun set-mustache-character (char new-function)
  (setf (gethash char *mustache-tag-table*) new-function))

(defun get-mustache-character (char)
  (declare (inline))
  (gethash char *mustache-tag-table*))

(defun make-tag (&key str escapep start end)
  (declare (type simple-string str)
           (type boolean escapep)
           (type offset start end))
  (let* ((tag-fun (get-mustache-character (char str 0)))
         (tag-text (string-trim '(#\Space #\Tab) str))
         (arg-text (string-trim '(#\Space #\Tab) (subseq tag-text 1))))
    (if tag-fun
        (funcall tag-fun str arg-text escapep start end)
        (make-instance 'normal-tag :text tag-text :escape escapep))))

(defmacro define-mustache-character (char &body body)
  `(set-mustache-character
    ,char (lambda (raw-text arg-text escapep start end)
            (declare (ignorable raw-text arg-text escapep start end)
                     (type simple-string raw-text arg-text)
                     (boolean escapep)
                     (offset start end))
            ,@body)))

(define-mustache-character #\&
  (make-instance 'ampersand-tag :text arg-text))

(define-mustache-character #\#
  (make-instance 'section-start-tag :text arg-text :end end
                                    :open-delimiter *open-delimiter*
                                    :close-delimiter *close-delimiter*))

(define-mustache-character #\^
  (make-instance 'section-start-tag :text arg-text :end end :falsey t))

(define-mustache-character #\/
  (make-instance 'section-end-tag :text arg-text :start start))

(define-mustache-character #\!
  (make-instance 'comment-tag :text ""))

(define-mustache-character #\=
  (let ((arg-text (string-trim '(#\Space #\Tab #\=) arg-text)))
    (prog1
        (make-instance 'delimiter-tag :text arg-text)
      (change-delimiter arg-text))))

(define-mustache-character #\>
  (make-instance 'partial-tag :text arg-text))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (text object) stream)))

;;; Lexer

;; Invariant token
(defvar beginning-of-line (make-instance 'beginning-of-line))
(defvar newline (make-instance 'newline))
(defvar crlf-newline (make-instance 'crlf-newline))

(defun string-starts-with-p (pattern string start)
  (declare (type string pattern string)
           (type offset start)
           (inline))
  (eql (string<= pattern string :start2 start)
       (length pattern)))

(defun read-text (type string start end)
  (declare (type symbol type)
           (type string string)
           (offset start end))
  (loop :for idx :from start :below end
        :while (case type
                 (text (text-char-p (char string idx)))
                 (whitespace (space-char-p (char string idx))))
        :until (string-starts-with-p *open-delimiter* string idx)
        :finally (return (values (make-instance type
                                                :text (subseq string start idx))
                                 idx))))

(defun read-newline (string start)
  (declare (type string string)
           (type offset start))
  (if (string-starts-with-p crlf string start)
      (values crlf-newline (+ 2 start))
      (values newline (1+ start))))

(defun read-tag (string triplep start end)
  (declare (type string string)
           (type boolean triplep)
           (type offset start end))
  (let ((before-tag start)
        (tag-open (if triplep *triple-open-delimiter* *open-delimiter*))
        (tag-close (if triplep *triple-close-delimiter* *close-delimiter*)))
    (when (string-starts-with-p tag-open string start)
      (incf start (length tag-open))
      (loop :for idx :from start :below end
            :until (string-starts-with-p tag-close string idx)
            :finally (let ((endpos (+ idx (length tag-close))))
                       (return (values (make-tag :str (subseq string
                                                              start idx)
                                                 :escapep (not triplep)
                                                 :start before-tag
                                                 :end endpos)
                                       endpos)))))))

(defun read-token (string start end)
  (declare (type string string)
           (type offset start end))
  (let ((char (char string start)))
    (cond
      ((space-char-p char)
       (read-text 'whitespace string start end))
      ((newline-char-p char)
       (read-newline string start))
      ((string-starts-with-p *triple-open-delimiter* string start)
       (read-tag string t start end))
      ((string-starts-with-p *open-delimiter* string start)
       (read-tag string nil start end))
      (t
       (read-text 'text string start end)))))

(defun scan (string &optional (start 0) (end (length string)))
  (declare (type string string)
           (type offset start end))
  (let ((*open-delimiter* *default-open-delimiter*)
        (*close-delimiter* *default-close-delimiter*))
    (loop :with idx :of-type offset := start
          :with token
          :while (> end idx)
          :when (zerop idx)
            :collect beginning-of-line
          :do (multiple-value-setq (token idx)
                (read-token string idx end))
          :collect token
          :when (and (< idx end)
                     (typep token 'newline))
            :collect beginning-of-line)))

;;; Parser

(deftype text-token ()
  '(and token
        (not (or beginning-of-line can-standalone-tag newline whitespace))))

(defun collect-line (tokens)
  (declare (type list tokens))
  (flet ((newlinep (token)
           (typep token 'newline)))
    (loop :for start := 0 :then (1+ finish)
          :for finish := (position-if #'newlinep tokens :start start)
          :when (subseq tokens start (and finish (1+ finish)))
            :collect it
          :until (null finish))))

(defun tokens-standalone-p (tokens)
  (declare (type list tokens))
  (when (eq (car tokens) beginning-of-line)
    (loop :for token :in tokens
          :count (typep token 'can-standalone-tag) :into tags
          :count (typep token 'text-token) :into texts
          :finally (return (and (= 1 tags)
                                (= 0 texts))))))

(defun find-standalone-tag (tokens)
  (declare (type list tokens))
  (flet ((tagp (token)
           (typep token 'tag)))
    (let* ((pos (position-if #'tagp tokens))
           (tag (elt tokens pos)))
      (setf (indent tag) (remove beginning-of-line
                                 (subseq tokens 0 pos)))
      tag)))

(defun trim-standalone (tokens)
  (declare (type list tokens))
  (loop :for line :in (collect-line tokens)
        :append (if (tokens-standalone-p line)
                    (list (find-standalone-tag line))
                    line)))

(defun make-section-tag (start-tag end-tag tokens)
  (declare (type tag start-tag end-tag)
           (type list tokens))
  (make-instance 'section-tag
                 :tokens tokens
                 :text (text start-tag)
                 :falsey (falsey start-tag)
                 :start (end start-tag)
                 :end (start end-tag)
                 :open-delimiter (open-delimiter start-tag)
                 :close-delimiter (close-delimiter start-tag)))

(defun group-sections (tokens &optional sections acc)
  (declare (type list tokens sections acc))
  (labels ((push-group (acc)
             (cons nil acc))
           (push-token (token acc)
             (cons (cons token (car acc)) (cdr acc)))
           (pop-group (acc)
             (cdr acc))
           (top-group (acc)
             (reverse (car acc)))
           (push-section-tag (start-tag end-tag acc)
             (push-token (make-section-tag start-tag end-tag (top-group acc))
                         (pop-group acc)))
           (tag-match (tag1 tag2)
             (string-equal (text tag1) (text tag2))))
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
             (group-sections rest sections (push-token token acc))))))))

(defun fold-text (tokens)
  (declare (type list tokens))
  (flet ((textp (token)
           (typep token 'text)))
    (loop :for start := 0 :then next
          :for finish := (position-if-not #'textp tokens :start start)
          :for next := (and finish (position-if #'textp tokens :start finish))
          :for texts := (subseq tokens start finish)
          :when texts
            :collect (make-instance 'text :text
                                    (format nil "~{~a~}" (mapcar #'text texts)))
          :when (and finish
                     (subseq tokens finish next))
            :append it
          :while next)))

(defun parse (template)
  (declare (inline))
  (group-sections (fold-text (trim-standalone (scan template)))))

;;; Context

(defvar *context* nil "Current context for lambda section")

(defclass context ()
  ((%data :initarg :data
          :initform nil
          :accessor data)
   (%indent :type list
            :initarg :indent
            :initform nil
            :accessor indent)
   (%partials :initarg :partials
              :initform nil
              :accessor partials)
   (%next :type (or null context)
          :initarg :next
          :initform nil
          :accessor next)))

(defun parse-key (string)
  (declare (type string string))
  (cond
    ((string= string ".") 'implicit-iterator)
    (t (loop :for start := 0 :then (1+ finish)
             :for finish := (position #\. string :start start)
             :collect (string-upcase (subseq string start finish))
             :until (null finish)))))

(defun key (token)
  (check-type token token)
  (parse-key (text token)))

(deftype alist ()
  '(cons (cons atom) (or cons null)))

(defun save-hash-table (source)
  (typecase source
    (null)
    (string source)
    (alist
     (loop :with table := (make-hash-table :test 'equal)
           :for (key . value) :in (reverse source)
           :do (setf (gethash (string-upcase key) table)
                     (save-hash-table value))
           :finally (return table)))
    (sequence
     (let ((result (map 'vector #'save-hash-table source)))
       (when (plusp (length result))
         result)))
    (otherwise source)))

(defun make-context-chain (data context)
  (declare (type context context))
  (let ((ctx (make-instance 'context)))
    (setf (data ctx) data
          (indent ctx) (indent context)
          (partials ctx) (partials context)
          (next ctx) context)
    ctx))

(defun ensure-context (maybe-context)
  "Ensure MAYBE-CONTEXT is a valid context. If not then make one."
  (ctypecase maybe-context
    (list (make-instance 'context :data (save-hash-table maybe-context)))
    (hash-table (make-instance 'context :data maybe-context))
    (context maybe-context)))

(defgeneric context-get (key context)
  (:documentation "Get data from CONTEXT by KEY."))

(defmethod context-get ((key string) context)
  (declare (ignore key))
  (values))

(defmethod context-get ((key string) (context null))
  (declare (ignore key))
  (values))

(defmethod context-get ((key string) (context hash-table))
  (gethash (string-upcase key) context))

(defmethod context-get ((key string) (context context))
  (multiple-value-bind (data find)
      (context-get key (data context))
    (if find
        (values data find)
        (when (next context)
          (context-get key (next context))))))

(defmethod context-get ((key (eql 'implicit-iterator)) context)
  (values (data context) t))

(defmethod context-get ((key list) context)
  (multiple-value-bind (data find)
      (context-get (car key) context)
    (if (cdr key)
        (context-get (cdr key) data)
        (values data find))))

;;; Partials

(defvar *load-path* (list *default-pathname-defaults*)
  "A list. The search pathes for partials.")
(defvar *default-pathname-type* "mustache"
  "The default file extension for partials.")

(defun filename (filename)
  (declare (type (or string pathname) filename))
  (or (uiop:file-exists-p filename)
      (uiop:file-exists-p (make-pathname :type *default-pathname-type*
                                         :defaults filename))))

(defun locate-file (filename)
  (declare (type (or string pathname) filename))
  (uiop:ensure-pathname filename :want-file t)
  (labels ((filename (path filename)
             (merge-pathnames
              path (make-pathname
                    :type *default-pathname-type*
                    :defaults filename)))
           (dir-file-exists-p (path)
             (uiop:file-exists-p (filename path filename))))
    (some #'dir-file-exists-p *load-path*)))

(defun read-partial (filename context)
  (declare (type (or string pathname) filename)
           (type context context))
  (let ((from-context (context-get filename (partials context))))
    (if from-context
        from-context
        (let ((pathname (locate-file filename)))
          (when pathname
            (uiop:read-file-string pathname))))))

;;; Rendering Utils

(defvar *escape-tokens* t)

(defun %escape (string)
  (flet ((needs-escape-p (char)
           (member char '(#\< #\> #\& #\\ #\" #\')))
         (escape-char (char)
           (case char
             (#\& "&amp;")
             (#\< "&lt;")
             (#\> "&gt;")
             (#\" "&quot;")
             (t (format nil "&#~d;" (char-code char))))))
    (with-output-to-string (datum)
      (loop :for start := 0 :then (1+ pos)
         :for pos := (position-if #'needs-escape-p string :start start)
         :do (write-string string datum :start start :end pos)
         :when pos
         :do (write-string (escape-char (char string pos)) datum)
         :while pos))))

(defun escape (string)
  "HTML escape STRING when *escape-tokens* is t."
  (declare (type string string))
  (if *escape-tokens*
      (%escape string)
      string))

(defvar *real-standard-output* (make-synonym-stream 'cl:*standard-output*))
(defvar *output-stream* (make-synonym-stream 'cl:*standard-output*)
  "The default output stream for mustache rendering. Bind this
variable before calling mustache-rendering and friends. Default is
*standard-output*.")

(defun %output ()
  (if (eq *mustache-output* *real-standard-output*)
      *output-stream*
      *mustache-output*))

(defgeneric print-data (data escapep context))

(defmethod print-data ((data string) escapep context)
  (declare (ignore context))
  (write-string (if escapep (escape data) data) (%output)))

(defmethod print-data ((data symbol) escapep context)
  (print-data (string data) escapep context))

(defmethod print-data ((data function) escapep context)
  (let ((*context* context))
    (let* ((value (format nil "~a" (funcall data)))
           (fun (compile-template value))
           (output (with-output-to-string (*output-stream*)
                     (funcall fun context))))
      (write-string (if escapep (escape output) output) (%output)))))

(defmethod print-data (token escapep context)
  (print-data (princ-to-string token) escapep context))

(defun call-lambda (lambda text context)
  (declare (type function lambda)
           (type string text)
           (type context context))
  (let ((*context* context))
    (let* ((value (format nil "~a" (funcall lambda text)))
           (fun (compile-template value))
           (output (with-output-to-string (*output-stream*)
                     (funcall fun context))))
      (write-string output (%output)))))

;;; Renderer

(defgeneric render-token (token context template))

(defmethod render-token ((token text) context (template string))
  (declare (ignore template))
  (print-data (text token) nil context))

(defmethod render-token ((token tag) context (template string))
  (declare (ignore template))
  (multiple-value-bind (dat find)
      (context-get (key token) context)
    (when find
      (print-data dat (escapep token) context))))

(defmethod render-token ((token partial-tag) context (template string))
  (let ((fun (compile-template
              (or (read-partial (text token) context) ""))))
    (push (indent token) (indent context))
    (funcall fun context)
    (pop (indent context))))

(defmethod render-token ((token section-tag) context (template string))
  (multiple-value-bind (ctx find)
      (context-get (key token) context)
    (when (or find (falsey token))
      (flet ((render (context template)
               (render-tokens (tokens token) context template)))
        (if (falsey token)
            (when (null ctx)
              (render (make-context-chain () context) template))
            (typecase ctx
              (hash-table
               (render (make-context-chain ctx context) template))
              (function
               (let ((*default-open-delimiter* (open-delimiter token))
                     (*default-close-delimiter* (close-delimiter token)))
                 (call-lambda ctx (subseq template (start token) (end token)) context)))
              ((and (not string) sequence)
               (map nil (lambda (ctx)
                          (render (make-context-chain ctx context) template))
                     ctx))
              (null)
              (t
               (render context template))))))))

(defmethod render-token ((token beginning-of-line) context (template string))
  (declare (ignore token))
  (render-tokens (car (indent context)) context template))

(defun render-tokens (tokens context template)
  (declare (type list tokens)
           (type context context)
           (type string template))
  (loop :for token :in tokens
        :do (render-token token context template)))

(defun render-body (tokens context template)
  (let ((context (ensure-context context)))
    (with-standard-io-syntax
      (render-tokens tokens context template))))

;;; Interfaces

(defun version ()
  "Return the CL-MUSTACHE version."
  #.(format nil "CL-MUSTACHE ~A (Mustache spec ~A)"
            (with-open-file (f (merge-pathnames "version.lisp-expr"
                                                (or *compile-file-pathname*
                                                    *load-truename*)))
              (read f))
            (with-open-file (f (merge-pathnames "spec-version.lisp-expr"
                                                (or *compile-file-pathname*
                                                    *load-truename*)))
              (read f))))

(defun make-context (&key data partials)
  "Create mustache context from alist DATA."
  (make-instance 'context :data (save-hash-table data)
                          :partials (save-hash-table partials)))

(defgeneric compile-template (template)
  (:documentation "Return a compiled rendering function."))

(defmethod compile-template ((template string))
  (let ((tokens (parse template)))
    (lambda (&optional context output-stream)
      (let ((*output-stream* (or output-stream *output-stream*)))
        (render-body tokens context template)))))

(defmethod compile-template ((template pathname))
  (let ((buffer (uiop:read-file-string (filename template))))
    (compile-template buffer)))

(defgeneric render (template &optional context output-stream)
  (:documentation
   "Render TEMPLATE with optional CONTEXT to *OUTPUT-STREAM* or OUTPUT-STREAM"))

(defmethod render ((template string) &optional context output-stream)
  (let ((*output-stream* (or output-stream *output-stream*)))
    (render-body (parse template) context template)))

(defmethod render ((template pathname) &optional context output-stream)
  (let ((buffer (uiop:read-file-string (filename template))))
    (render buffer context output-stream)))

(defun render* (template &optional context)
  "Render TEMPLATE with optional CONTEXT to string."
  (with-output-to-string (out)
    (render template context out)))

(defmacro define (name template)
  "Define a named renderer of string TEMPLATE."
  `(setf (symbol-function ',name)
         (compile-template ,template)))

;;; mustache.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

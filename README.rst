========
CL-MUSTACHE
========

Inspired by ctemplate_ and et_, Mustache_ is a
framework-agnostic way to render logic-free views.

As ctemplates says, "It emphasizes separating logic from presentation:
it is impossible to embed application logic in this template language."

CL-MUSTACHE is a Common Lisp implementation of Mustache v1.1.2+λ. Tested with:

 - SBCL 1.0.55
 - CLISP 2.49

CL-MUSTACHE is semantically versioned: http://semver.org.

Documentation
=============

The different Mustache tags are documented at `mustache(5)`_.

Install It
==========

Using quicklisp is recommended.

::

    CL-USER> (ql:quickload "cl-mustache")


Use It
======

Currently accepts context data in alist format, for example:

::

   `((:tag . "string")
     (:array . #(1 2 3 4))
     (:lambda ,(lambda () "world"))
     (:nested . ((:data . t))))

To render the template:

::

    CL-USER> (mustache-render "Hi {{person}}!" '((:person . "Mom")))
    "Hi Mom!"

Or save the renderer for later use:

::

    CL-USER> (setf view (mustache-compile "Hi {{person}}!"))

Or define static renderer function:

::

    CL-USER> (defmustache view "Hi {{person}}!")
    CL-USER> (view context)

Test It
=======

::

    CL-USR> (ql:quickload "cl-mustache-test")
    CL-USR> (mustache-test:run-test)
    ;; or
    CL-USR> (mustache-test:start-test-server)

Extend It (Experimental)
========================

Define your tag classes, tag character and render function:

::

    (in-package :mustache)
    (defclass exec-tag (non-standalone-tag)
      ((command :initarg :command :accessor command)))
    (set-mustache-character
      #\$
      (lambda (raw-text arg-text escapep start end)
        (make-instance 'exec-tag :command arg-text)))
    ;; or
    ;; (define-mustache-character #\$
    ;;   (make-instance 'exec-tag :command arg-text))
    (defmethod render-token ((token exec-tag) context template)
       (print-data (run-program-output (command token)) t context))

.. _ctemplate: http://code.google.com/p/google-ctemplate/
.. _et: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
.. _Mustache: http://mustache.github.com/
.. _mustache(5): http://mustache.github.com/mustache.5.html

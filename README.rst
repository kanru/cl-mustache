========
CL-MUSTACHE
========

Inspired by ctemplate_ and et_, Mustache_ is a
framework-agnostic way to render logic-free views.

As ctemplates says, "It emphasizes separating logic from presentation:
it is impossible to embed application logic in this template language."

CL-MUSTACHE is a Common Lisp implementation of Mustache v1.1.2. Tested with:

 - SBCL 1.0.55
 - CLISP 2.49

CL-MUSTACHE is semantically versioned: http://semver.org.

Documentation
=============

The different Mustache tags are documented at `mustache(5)`_.

Install It
==========

In the future

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

    CL-USER> (setf view (mustache-compile "Hi {{person"}}!"))

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

.. _ctemplate: http://code.google.com/p/google-ctemplate/
.. _et: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
.. _Mustache: http://mustache.github.com/
.. _mustache(5): http://mustache.github.com/mustache.5.html

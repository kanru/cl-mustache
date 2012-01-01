========
CL-MUSTACHE
========

Inspired by ctemplate_ and et_, Mustache_ is a
framework-agnostic way to render logic-free views.

As ctemplates says, "It emphasizes separating logic from presentation:
it is impossible to embed application logic in this template language."

CL-MUSTACHE is a Common Lisp implementation of Mustache. Tested with:

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

::

    CL-USER> (mustache-render "Hi {{person}}!" '((:person . "Mom")))
    "Hi Mom!"

Test It
=======

::

    CL-USR> (ql:quickload "cl-mustache-test")
    CL-USR> (mustache-test:run-test)

.. _ctemplate: http://code.google.com/p/google-ctemplate/
.. _et: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
.. _Mustache: http://mustache.github.com/
.. _mustache(5): http://mustache.github.com/mustache.5.html

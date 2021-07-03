(asdf:defsystem cl-mustache
  :version #.(with-open-file (stream (merge-pathnames "version.lisp-expr"
                                                      *load-truename*))
               (read stream))
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "Mustache Template Renderer"
  :components ((:file "packages")
               (:file "mustache" :depends-on ("packages"))
               (:file "compat-api-v1" :depends-on ("mustache")))
  :depends-on ("uiop")
  :in-order-to ((asdf:test-op (asdf:load-op :cl-mustache-test)))
  :perform (asdf:test-op (o c)
                         (asdf/package:symbol-call :prove 'run :cl-mustache-test)))

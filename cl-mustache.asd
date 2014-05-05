(asdf:defsystem cl-mustache
  :version #.(with-open-file (f (merge-pathnames "version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "Mustache Template Renderer"
  :components ((:file "packages")
               (:file "mustache" :depends-on ("packages"))
               (:file "compat-api-v1" :depends-on ("mustache")))
  :depends-on ("uiop" "alexandria"))

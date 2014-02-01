(asdf:defsystem cl-mustache
  :version "0.10.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "Mustache Template Renderer"
  :components ((:file "packages")
               (:file "mustache" :depends-on ("packages"))
               (:file "compat-api-v1" :depends-on ("mustache")))
  :depends-on ("cl-fad" "alexandria"))

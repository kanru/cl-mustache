(asdf:defsystem cl-mustache
  :version "0.9.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "Mustache Template Renderer"
  :components ((:file "packages")
               (:file "mustache" :depends-on ("packages")))
  :depends-on ("cl-fad" "alexandria"))

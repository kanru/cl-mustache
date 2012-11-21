(asdf:defsystem cl-mustache-test
  :version "2.0.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "base"
                :pathname ""
                :components ((:file "t/test")))
               (:module "t"
                :depends-on ("base")
                :components ((:file "spec"))))
  :depends-on ("cl-mustache"
               "cl-test-more"))

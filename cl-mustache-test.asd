(asdf:defsystem cl-mustache-test
  :version "2.0.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "t"
                :components
                ((:module "base"
                  :pathname ""
                  :components ((:file "test")))
                 (:module "tests"
                  :pathname ""
                  :depends-on ("base")
                  :components ((:file "test-spec")
                               (:file "test-api"))))))
  :depends-on ("cl-mustache"
               "cl-test-more"))

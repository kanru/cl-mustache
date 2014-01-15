(asdf:defsystem cl-mustache-test
  :version "2.0.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "t"
                :components
                ((:file "test")
                 (:module "tests"
                  :pathname ""
                  :depends-on ("test")
                  :components ((:file "test-spec")
                               (:file "test-api")
                               (:static-file "test.mustache"))))))
  :depends-on ("cl-mustache"
               "clunit"))

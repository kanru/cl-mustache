(asdf:defsystem cl-mustache-test
  :version "2.0.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "Test for CL-MUSTACHE"
  :depends-on ("cl-mustache" "prove")
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                :components
                ((:test-file "test-spec")
                 (:test-file "test-api")
                 (:static-file "test.mustache")))))

(asdf:defsystem cl-mustache-test
  :version "1.0.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:file "test"))
  :depends-on ("cl-mustache"
               "cl-json"
               "cl-markup"
               "cl-fad"
               "toot"))

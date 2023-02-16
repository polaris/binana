;;;; binana.asd

(asdf:defsystem #:binana
  :description "Binary analysis tools"
  :author "Jan Deinhard <jan.deinhard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "binana"))
  :in-order-to ((test-op (test-op "binana/tests"))))

(asdf:defsystem #:binana/tests
  :depends-on (#:binana
               #:fiveam)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "main"))))
  :perform (test-op (o c)
                    (uiop:symbol-call '#:fiveam '#:run! :binana/tests)))


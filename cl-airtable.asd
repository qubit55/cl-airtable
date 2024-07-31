(defsystem "cl-airtable"
  :version "0.0.1"
  :author "Anton Lobach"
  :mailto "antonlobach@uri.com"
  :license "MIT"
  :depends-on ("dexador"
               "arrow-macros")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-airtable/tests"))))

(defsystem "cl-airtable/tests"
  :author "Anton Lobach"
  :license "MIT"
  :depends-on ("cl-airtable"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-airtable"
  :perform (test-op (op c) (symbol-call :rove :run c)))

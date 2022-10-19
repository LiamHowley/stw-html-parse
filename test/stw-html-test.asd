(defsystem #:stw-html-test
  :description "Test suite for stw-html-parse."
  :depends-on ("parachute" "stw-xml-parse" "stw-html-parse")
  :serial t
  :components ((:file "package")
	       (:file "tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :html.test)))

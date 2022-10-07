(defsystem #:stw-html-parse
  :depends-on ("contextl"
	       "closer-mop"
	       "stw-xml-parse"
	       "stw-utils")
  :description "A HTML to DOM style parser; parsing elements and attributes into CLOS objects."
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "meta")
	       (:file "model")
	       (:file "parse")
	       (:file "print")
	       (:file "query")))

(defsystem #:stw-html-parse
  :depends-on ("contextl"
	       "closer-mop"
	       "stw-xml-parse"
	       "stw-svg-parse"
	       "stw-mathml-parse"
	       "stw-utils"
	       "cl-comp")
  :description "A HTML to DOM style parser; parsing elements and attributes into CLOS objects."
  :serial t
  :components ((:file "package")
	       (:file "meta")
	       (:file "model")
	       (:file "parse")
	       (:file "print")
	       (:file "read")
	       (:file "query"))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "docs/README.org"))
  :in-order-to ((test-op (load-op :stw-html-test))))

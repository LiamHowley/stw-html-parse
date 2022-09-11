(in-package :cl-user)

(defpackage html.parse
  (:use :cl :xml.parse)
  (:nicknames :html)

  (:import-from
   :contextl
   :singleton-class)

  (:import-from
   :stw.util
   :ensure-list)

  (:import-from
   :stw.util
   :stw-read-char
   :next
   :match-string
   :match-character
   :read-until
   :read-and-decode)

  (:import-from
   :stw.util
   :*char-index*
   :*document*
   :*length*
   :match-string
   :match-character
   :read-until
   :read-and-decode)

  (:import-from
   :closer-mop
   :effective-slot-definition-class
   :direct-slot-definition-class
   :compute-effective-slot-definition
   :validate-superclass)

  (:export

   ;; specials
   :*preserve-whitespace*
   :*element-name*
   :*next-attribute*
   :*next-element*
   :*element-tags*
   :*end-sgml*
   :*end-comment*
   :*end-cdata*

   ;; meta
   :element-class
   :xml-direct-slot-definition

   ;; model
   :define-element-node
   :document-node
   :xml-document-node
   :html-document-node
   :dom-node
   :standard-element-node
   :element-node
   :branch-node
   :leaf-node
   :content-node
   :text-node
   :whitespace-node

   :sgml-node
   :?xml
   :!--
   :![CDATA[
   :!DOCTYPE

   ;; functions
   :parse-document
   :read-from-file
   :read-element
   :read-element-name
   :read-element-attributes
   :read-attribute
   :read-attribute-value
   :read-content
   :read-into
   :read-subelements
   :bind-child-node
   :prepare-slot
   :assign-value
   :parse-value
   :get-element-name
   :map-attribute-to-slot
   :map-attribute
   :tag-open-char
   :serialize
   :write-to-file))
   

(in-package html.parse)

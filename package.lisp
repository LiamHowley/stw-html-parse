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
   :write-to-file)

  (:export
   :find-ancestor-node
   :clone-node
   :walk-tree
   :retrieve-text-nodes
   :retrieve-text-nodes-with-token
   :retrieve-text-nodes-with-tokens
   :retrieve-text-nodes-with-all-tokens
   :get-elements-by-tagname
   :get-element-with-attribute
   :get-element-with-attributes
   :get-element-with-attribute-value
   :get-element-with-attribute-values
   :get-elements-with-attribute
   :get-elements-with-attributes
   :get-elements-with-attribute-value
   :get-elements-with-attribute-values
   :get-next-sibling
   :get-previous-sibling
   :query-select
   :query-select-all
   :attribute-value
   :remove-node
   :add-node
   :insert-before
   :insert-after
   :first-of-type
   :last-of-type
   :get-elements-by-class
   :get-element-by-id))

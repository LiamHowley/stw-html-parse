(in-package html.test)

(defvar *markup* "<div id='container1' class='container square'><a href=\"/test\" target=\"_blank\"><span class='square'>caption</span><img src='/my-img-server'/>><</a></div>")

(defvar *parsed-markup* (parse-document *markup* :element-class-map *html-element-class-map*))

(define-test parse-markup...
  :parent test-parse
    (of-type 'document-node *parsed-markup*)
  (let ((element (car (get-elements-by-tagname *parsed-markup* "a" *html-element-class-map*))))
    (of-type 'html.parse::a element)
    (of-type 'html-element-class (class-of element)))
  (of-type 'element-node (car (slot-value *parsed-markup* 'child-nodes)))
  (of-type 'html.parse::div (get-element-with-attribute *parsed-markup* "id"))
  (of-type 'html.parse::div (get-element-with-attribute-value *parsed-markup* "id" "container1"))
  ;; this is html where attribute values can only be a string. This should be false
  (let ((element (get-element-with-attribute-value *parsed-markup* "class" "container")))
    (true (typep element 'html.parse::div)))
  (let ((element (get-element-with-attribute-value *parsed-markup* "class" "container" "square")))
    (true (typep element 'html.parse::div)))
  (let ((elements (get-elements-with-attribute-value *parsed-markup* "class" "square")))
    (of-type 'html.parse::span (cadr elements)))
  (let ((elements (get-elements-with-attribute-values *parsed-markup* "class" "square" "container")))
    (of-type 'html.parse::div (car elements)))
  (let ((text-nodes (retrieve-text-nodes *parsed-markup*)))
    (is string= "caption" (text (car text-nodes)))
    (is string= ">" (text (cadr text-nodes)))
    (is string= "<" (text (caddr text-nodes)))
    (of-type 'html.parse::span (parent-node (car text-nodes)))
    (of-type 'html.parse::img (get-next-sibling (parent-node (car text-nodes)))))
  (is string= "caption" (text (car (retrieve-text-nodes-with-token *parsed-markup* "cap"))))
  (is string= "caption" (text (car (retrieve-text-nodes-with-tokens *parsed-markup* "cap" "tion")))))



(define-test serialize...
  :parent test-parse
  (is string= "<div id='container1' class='container square'><a href='/test' target='_blank'><span class='square'>caption</span><img src='/my-img-server' />&gt;&lt;</a></div>" (serialize *parsed-markup*)))

(define-test reader...
  :parent test-parse
  (when (readerp)
    (remove-reader))
  (true (set-reader #'read-html))
  (let* ((document-node (read-from-string "<input type=\"text\" id=\"name\" name=\"name\" required minlength=\"4\" maxlength=\"8\" size=\"10\">"))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (true (slot-exists-p child-node 'html.parse::name))
    (true (html-parse-required child-node))
    (false (html-parse-autocomplete child-node))
    (is string= "text" (input-type child-node))
    (is string= "<input id='name' type='text' maxlength='8' minlength='4' name='name' required size='10' />"
	(write-to-string document-node))
    (of-type 'readtable (remove-reader))))


(define-test errors-and-generic-nodes...
  :parent test-parse
  (setf *mode* :strict)
  (fail (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>"
			:element-class-map *html-element-class-map*)
      'class-not-found-error)
  (setf *mode* :silent)
  (let* ((document-node (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>"
					:element-class-map *html-element-class-map*))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (of-type 'generic-node child-node)
    (is string= "a custom node" (text (car (retrieve-text-nodes-with-token document-node "a custom node"))))
    (of-type 'generic-node (get-element-with-attribute document-node "custom-slot"))))

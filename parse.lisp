(in-package html.parse)


(defmethod map-attribute ((res (eql 'aria-*)) attribute length)
  (declare (ignore attribute length res))
  (read-until (match-character #\space #\= #\> #\/)))

(defmethod map-attribute ((res (eql 'data-*)) attribute length)
  (declare (ignore attribute length res))
  (read-until (match-character #\space #\= #\> #\/)))

(defmethod map-attribute ((res (eql 'event-*)) attribute length)
  (declare (ignore attribute length res))
  (read-until (match-character #\space #\= #\> #\/)))

(defmethod generic-attribute ((class html-element-class))
  (values (find-slot-definition class
				'generic-attribute
				'html-direct-slot-definition)
	  'generic-attribute))

;; assigning values

(defmethod assign-value ((class element-node) (slot-name (eql 'event-*)) attribute value)
  (with-slots (event-*) class
    (setf (slot-value event-* (find-symbol attribute 'html.parse)) value)))

(defmethod assign-value ((class element-node) (slot-name (eql 'aria-*)) attribute value)
  (push (cons attribute value) (slot-value class 'aria-*)))

(defmethod assign-value ((class element-node) (slot-name (eql 'data-*)) attribute value)
  (push (cons attribute value) (slot-value class 'data-*)))


(defmethod read-attribute-value
    ((slot html-direct-slot-definition) attribute slot-type)
  (declare (inline match-character stw-read-char))
  (let ((char (stw-read-char)))
    (when char
      (case char
	(#\=
	 (next)
	 (read-attribute-value slot attribute slot-type))
	((#\" #\')
	 (next)
	 (let ((predicate (attribute-value-predicate char slot-type)))
	   (cond ((eq slot-type 'boolean)
		  (let ((value (read-until predicate)))
		    (the boolean (string-equal value attribute))))
		 (t
		  (prog1 
		      (let ((value (read-into slot-type predicate)))
			(if (char= (stw-read-char) char)
			    value
			    (restart-case
				(multiple-value-error "The attribute ~a does not support multiple values" attribute)
			      (use-value (user-supplied)
				user-supplied)
			      (use-first-found-value ()
				:report "Use first found value and skip the rest"
				(consume-until (match-character char))
				value)
			      (ignore-attribute ()
				:report "Ignore all values."
				(consume-until (match-character char))
				nil))))
		    (next))))))
	((#\space #\> #\newline #\return #\linefeed)
	 (the boolean 
	      (cond ((eq slot-type 'boolean)
		     t)
		    (t nil))))
	(t 
	 (let ((predicate (attribute-value-predicate char slot-type)))
	   (read-into slot-type predicate)))))))

(defmethod read-subelements ((node html))
  (let ((*embedded* t))
    (call-next-method)))

(defmethod read-fragment ((node document-node))
  (let ((*case-sensitive* nil))
    (read-subelements node)))


;; embedded setup

(defmethod initialize-node ((node svg) filter)
  "As there are overlapping/duplicate element names/classes between svg and html
   we need to specify the correct class map for reading."
  (let ((*element-class-map* *embedded-svg-class-map*)
	(*case-sensitive* t))
    (call-next-method)))

(defmethod initialize-node ((node math) filter)
  "As there are overlapping/duplicate element names/classes between math and html
   we need to specify the correct class map for reading."
  (let ((*element-class-map* *embedded-mathml-class-map*)
	(*case-sensitive* t))
    (call-next-method)))

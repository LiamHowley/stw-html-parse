(in-package html.parse)


(defmethod map-attribute ((res (eql 'aria-*)) attribute length)
  (declare (ignore length res))
  (read-until (match-character #\space #\= #\> #\/)))


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
	((#\space #\>)
	 (the boolean 
	      (cond ((eq slot-type 'boolean)
		     t)
		    (t nil))))
	(t 
	 (let ((predicate (attribute-value-predicate char slot-type)))
	   (read-into slot-type predicate)))))))


(defmethod read-fragment ((node document-node))
  (let ((char (stw-read-char)))
    (case char
      (#\<
       (case (stw-peek-next-char)
	 (#\/
	  (read-content node))
	 (t
	  (next)
	  (let ((fragment (read-into-object)))
	    (when (typep fragment 'element-node)
	      (next))
	    (bind-child-node node fragment)))))
      (t
       (read-content node)))))

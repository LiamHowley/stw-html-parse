(in-package html.parse)

(set-reader-function :end-conditional (read-until (match-string "<![endif]-->")))

(set-reader-function :end-title (read-until (match-string "</title>" nil)))

(set-reader-function :end-script (read-until (match-string "</script>" nil)))

(set-reader-function :end-style (read-until (match-string "</style>" nil)))

(set-reader-function :end-textarea (read-until (match-string "</textarea>" nil)))


(defmethod parse-document ((document html-document-node) &key (parser #'read-element) preserve-whitespace)
  (declare (ignorable parser preserve-whitespace))
  (let ((*element-class-map* *html-element-class-map*))
    (call-next-method)))

(defmethod map-attribute ((res (eql 'aria-*)) attribute length)
  (declare (ignore length)
	   (ignore res))
;; slots

(defmethod prepare-slot
    ((class element-node) (slot (eql 'event-*)))
  (unless (slot-boundp class 'event-*)
    (setf (slot-value class 'event-*) (make-instance 'global-event-attribute))))
  (call-reader :next-attribute))


;; assigning values

(defmethod assign-value ((class element-node) (slot-name (eql 'event-*)) attribute value)
  (with-slots (event-*) class
    (setf (slot-value event-* (find-symbol attribute 'html.parse)) value)))

(defmethod assign-value ((class element-node) (slot-name (eql 'aria-*)) attribute value)
  (push (cons attribute value) (slot-value class 'aria-*)))

(defmethod assign-value ((class element-node) (slot-name (eql 'data-*)) attribute value)
  (push (cons attribute value) (slot-value class 'data-*)))



(declaim (ftype (function (character type) keyword) attribute-value-reader)
	 (inline attribute-value-reader))

(defun attribute-value-reader (char type)
  (declare (optimize (speed 3) (safety 0)))
  (case type
    ((cons list array)
     (ecase char
       (#\' :read-until-single-quote)
       (#\" :read-until-double-quote)))
    (t
     (case char
       (#\' :read-until-single-quote)
       (#\" :read-until-double-quote)
       (t :attribute-delimiter)))))


(defmethod read-attribute-value
    ((slot html-direct-slot-definition) attribute slot-type)
  (declare (inline match-character stw-read-char))
  (let ((char (stw-read-char)))
    (when char
      (let ((reader (attribute-value-reader char slot-type)))
	(case char
	  (#\=
	   (next)
	   (read-attribute-value slot attribute slot-type))
	  ((#\" #\')
	   (next)
	   (cond ((eq slot-type 'boolean)
		  (let ((value (call-reader reader)))
		    (the boolean (string-equal value attribute))))
		 (t
		  (prog1 
		      (let ((value (read-into slot-type reader)))
			(if (char= (stw-read-char) char)
			    value
			    (restart-case
				(multiple-value-error "The attribute ~a does not support multiple values" attribute)
			      (use-value (user-supplied)
				user-supplied)
			      (use-first-found-value ()
				:report "Use first found value and skip the rest"
				(funcall (consume-until (match-character char)))
				value)
			      (ignore-attribute ()
				:report "Ignore all values."
				(funcall (consume-until (match-character char)))
				nil))))
		    (next)))))
	  ((#\space #\>)
	   (the boolean 
		(cond ((eq slot-type 'boolean)
		       t)
		      (t nil))))
	  (t 
	   (read-into slot-type reader)))))))


(defmethod read-fragment ((node document-node))
  (let* ((char (stw-read-char)))
    (case char
      (#\<
       (next)
       (let ((fragment (read-into-object)))
	 (when (typep fragment 'element-node)
	   (next))
	 (bind-child-node node fragment)))
      (t
       (read-content node)))))

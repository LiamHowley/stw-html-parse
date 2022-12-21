(in-package html.parse)


;;; printing

;;; print attributes

;;; print global event attributes

(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'global-event-attribute)) (stream stream))
  (let* ((event-attributes-object (slot-value object (slot-definition-name slot)))
	 (class (class-of event-attributes-object)))
    (dolist (slot-definition (filter-slots-by-type class 'standard-direct-slot-definition))
      (let ((slot-name (slot-definition-name slot-definition)))
	(when (slot-boundp event-attributes-object slot-name)
	  (print-slot event-attributes-object slot-definition 'string stream))))))


(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'boolean)) (stream stream))
  (write-string " " stream)
  (write-string (slot-definition-attribute slot) stream))

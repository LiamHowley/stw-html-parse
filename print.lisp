(in-package html.parse)


;;; printing

;;; print attributes

(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'multiple-attributes)) (stream stream))
  (loop
    for (attribute . value) in (slot-value object (slot-definition-name slot))
    do (write-string " " stream)
    do (write-string (string-downcase attribute) stream)
    do (write-string "='" stream)
    do (write-string value stream)
    do (write-string "'" stream)))
  


;;; print global event attributes
(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'global-event-attribute)) (stream stream))
  (let* ((slot-value (slot-value object (slot-definition-name slot)))
	 (class (class-of slot-value)))
    (dolist (slot-definition (filter-slots-by-type (class-of object) 'html-direct-slot-definition))
      (let ((slot-name (slot-definition-name slot-definition)))
	(print-slot slot-value definition (slot-definition-type slot-definition) stream)))))


;; xml attributes cannot have multiple values. However, html attributes can:
(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'cons)) (stream stream))
  ;; attribute name
  (write-string " " stream)
  (write-string (slot-definition-attribute slot) stream)
  (write-string "='" stream)
  ;; attribute values
  (let ((slot-name (slot-definition-name slot)))
    (let ((values (slot-value object slot-name)))
      (loop for value in values
	    do (write-string value stream)
	    if (equal value (car (last values)))
	      do (write-string "'" stream)
	    else do (write-string " " stream)))))

(defmethod print-slot ((object dom-node) (slot html-direct-slot-definition) (type (eql 'boolean)) (stream stream))
  (write-string " " stream)
  (write-string (slot-definition-attribute slot) stream))

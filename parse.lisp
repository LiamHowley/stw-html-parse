(in-package html.parse)


(defmethod parse-document ((document html-document-node) &key (parser #'read-element) preserve-whitespace)
  (let ((*end-conditional* (read-until (match-string "<![endif]-->")))
	(*end-title* (read-until (match-string "</title>" nil)))
	(*end-script* (read-until (match-string "</script>" nil)))
	(*end-style* (read-until (match-string "</style>" nil)))
	(*element-class-map* *html-element-class-map*))
    (call-next-method)))
	

(defmethod read-content ((node !--[if))
  (with-slots (the-content) node
    (setf the-content (funcall *end-conditional*))))



(defmethod map-attribute ((res (eql 'aria-*)) attribute length)
  (declare (ignore length)
	   (ignore res))
  (funcall *next-attribute*))


;; slots

(defmethod prepare-slot
    ((class element-node) (slot (eql 'event-*)))
  (unless (slot-boundp class 'event-*)
    (setf (slot-value class 'event-*) (make-instance 'global-event-attribute))))


;; assigning values

(defmethod assign-value
    ((class element-node) (slot html-direct-slot-definition) slot-name attribute value)
  (unless (assign-slot-value class slot-name attribute value)
    (call-next-method)))


(defmethod assign-slot-value ((class element-node) (slot (eql 'event-*)) attribute value)
  (declare (ignore slot))
  (with-slots (event-*) class
    (setf (slot-value event-* (find-symbol attribute 'html.parse)) value)))

(defmethod assign-slot-value ((class element-node) (slot (eql 'aria-*)) attribute value)
  (push (cons attribute value) (slot-value class 'aria-*)))


(defmethod read-attribute-value ((slot html-direct-slot-definition) attribute slot-type)
  (declare (inline match-character))
  (let* ((char (stw-read-char))
	 (reader (read-and-decode (match-character char))))
    (case char
      (:eof nil)
      (#\=
       (next)
       (read-attribute-value slot attribute slot-type))
      ((#\" #\')
       (next)
       (funcall reader))
      ((#\newline #\space #\>)
       nil)
      (t 
       (funcall
	(read-and-decode #'(lambda (test-char)
			     (when (or (char= test-char #\space)
				       (char= test-char #\>))
			       test-char))))))))


(defmethod read-attribute-value
    ((slot html-direct-slot-definition) attribute slot-type)
    (declare (inline match-character))
    (let ((char (stw-read-char)))
      (when char
	(let* ((type (parse-type slot-type))
	       (predicate (match-character char))
	       (reader (read-until predicate)))
	  (case char
	    (#\=
	     (next)
	     (read-attribute-value slot attribute slot-type))
	    ((#\" #\')
	     (next)
	     (cond ((eq type 'boolean)
		    (let ((value (funcall (read-until predicate))))
		      (the boolean (string-equal value attribute))))
		   (t
		    (prog1 (read-into type predicate)
		      (next)))))
	    ((#\space #\>)
	     (the boolean 
		  (cond ((eq type 'boolean)
			 t)
			(t nil))))
	    (t 
	     (read-into type #'(lambda (test-char)
				 (member test-char '(#\space #\> #\/) :test #'char=)))))))))


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



(defmethod parse-fragment ((node document-node))
  (declare (inline parse%))
  (let ((*preserve-whitespace* t))
    (parse% #'read-fragment node)))



(defun parse-type% (type)
  "returns non null type"
  (declare (optimize (speed 3) (safety 0)))
  (typecase type
    (atom type)
    (cons
     (if (eq (car type) 'null)
	 (cadr type)
	 (car type)))))


(declaim (inline parse-type))

(defun parse-type (type)
  "returns type"
  (declare (optimize (speed 3) (safety 0))
	   (inline parse-type%))
  (case (parse-type% type)
    ((integer real)
     'fixnum)
    (t type)))

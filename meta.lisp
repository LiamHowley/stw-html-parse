(in-package html.parse)

(defclass html-element-class (element-class)
  ((status :initform :active
	   :initarg :status
	   :reader status)
   (tag-omission :initform :none
		 :initarg :tag-omission
		 :reader tag-omission)
   (implicit-aria :initform nil
		  :initarg :implicit-aria
		  :reader implicit-aria)
   (permitted-aria :initform nil 
		   :initarg :permitted-aria
		   :reader permitted-aria)))

(defmethod initialize-instance :after ((class html-element-class) &key)
  (setf (slot-value class 'element) (string-downcase (symbol-name (class-name class)))))

(defclass html-direct-slot-definition (xml-direct-slot-definition)
  ()
  (:documentation "default class representing html attribute slots"))

(defclass html-effective-slot-definition (xml-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class
    ((class html-element-class) &key &allow-other-keys)
  (find-class 'html-direct-slot-definition))

(defmethod effective-slot-definition-class
    ((class html-element-class) &key &allow-other-keys)
  (call-next-method))

(defmethod compute-effective-slot-definition
    ((class html-element-class) name direct-slot-definitions)
  (call-next-method))

(defmethod validate-superclass
    ((class html-element-class)
     (superclass element-class))
  t)

(defmethod validate-superclass
    ((superclass element-class)
     (class html-element-class))
  t)

(defvar *html-element-class-map*
  (let ((table (make-hash-table :test #'equal)))
    (maphash #'(lambda (element class)
		 (setf (gethash element table) class))
	     *element-class-map*)
    table)
  "Copying the elements from XML.PARSE:*ELEMENT-CLASS-MAP* 
as they may well be called upon during parsing.")

(defmethod shared-initialize :around ((class html-element-class) slot-names &key)
  (declare (ignore slot-names))
  ;; as there may be overlapping/duplicate element names/classes between differening xml schemas
  ;; we need to specify the correct hash-table for writing.
  (let ((*element-class-map* *html-element-class-map*))
    (call-next-method)))

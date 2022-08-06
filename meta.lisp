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

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

(defvar *html-element-class-map* (make-hash-table :test #'equal))

(defvar *embedded-svg-class-map* (make-hash-table :test #'equal))

(defvar *embedded-mathml-class-map* (make-hash-table :test #'equal))

(defmethod shared-initialize :around ((class html-element-class) slot-names &key)
  (declare (ignore slot-names))
  ;; as there may be overlapping/duplicate element names/classes between differening xml schemas
  ;; we need to specify the correct hash-table for writing.
  (let ((*element-class-map* *html-element-class-map*))
    (call-next-method)
    (setf (gethash "svg" *html-element-class-map*) (find-class 'svg)
	  (gethash "math" *html-element-class-map*) (find-class 'math))
    (merge-hash-tables *embedded-svg-class-map*
		       *svg-element-class-map*
		       *html-element-class-map*)
    (merge-hash-tables *embedded-mathml-class-map*
		       *mathml-element-class-map*
		       *html-element-class-map*)))

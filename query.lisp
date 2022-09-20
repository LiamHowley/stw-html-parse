(in-package html.parse)

(defgeneric get-elements-by-class (node &rest classlist)
  (:documentation "Return all elements that contain each class in classlist")
  (:method (node &rest classlist)
      (apply #'get-elements-with-attribute-values node "class" classlist)))


(defgeneric get-element-by-id (node id)
  (:documentation "Return first element with the specified id")
  (:method (node id)
      (get-element-with-attribute-value node "id" id)))

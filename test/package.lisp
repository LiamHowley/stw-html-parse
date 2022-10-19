(defpackage html.test
  (:use :cl :parachute :xml.parse :html.parse)
  (:shadow :output :body :summary :value)
  (:shadowing-import-from
   :parachute
   :is)
  (:shadowing-import-from
   :html.parse
   :name)
  (:export :test-parse))

(in-package html.test)

(define-test test-parse)

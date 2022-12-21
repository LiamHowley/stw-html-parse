(defpackage html.test
  (:use :cl :parachute :xml.parse :html.parse)
  (:shadowing-import-from
   :parachute
   :body
   :output
   :summary)
  (:export :test-parse))

(in-package html.test)

(define-test test-parse)

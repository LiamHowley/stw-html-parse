(defpackage html.test
  (:use :cl :parachute :xml.parse :html.parse)
  (:export :test-parse))

(in-package html.test)

(define-test test-parse)

(in-package html.parse)
  
(defun read-html (stream char)
  (declare (ignore char))
  (let ((next-char (read-char stream nil nil)))
    (case next-char
      (#\space
       (unread-char next-char stream)
       (return-from read-html (values (intern "<"))))
      (#\=
       (return-from read-html (values (intern "<="))))
      (t
       (unread-char next-char stream)
       (let ((output (with-output-to-string (out)
		       (write-char #\< out)
		       (parse-stream stream out))))
	 (parse-document (make-instance 'html-document-node :document output)))))))

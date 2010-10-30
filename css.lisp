(in-package :cl-css)

(defun format-directive (d) (format nil "~(~a { ~{~a: ~a; ~}}~)~%" (car d) (cdr d)))

(defun css (directives)
  (if (= 1 (length directives))
      (format-directive (car directives))
      (reduce (lambda (a b)
		(let ((final-a (if (listp a) (format-directive a) a)))
		  (concatenate 'string final-a  (format-directive b))))
	      directives)))
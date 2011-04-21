(in-package :cl-css)

(defun format-directive (d) (format nil "~(~a { ~{~a: ~a; ~}}~)~%" (car d) (cdr d)))
(defun format-inline-directive (d) (format nil "~({ ~{~a: ~a; ~}}~)~%" (cdr d)))

(defun css (directives)
  (if (= 1 (length directives))
      (format-directive (car directives))
      (reduce (lambda (a b) (concatenate 'string a (format-directive b)))
	      (cons "" directives))))

(defun inline-css (directive) (format nil "~(~{~a: ~a;~^ ~}~)" directive))

(defun compile-css (file-path directives)
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (format stream (css directives))))
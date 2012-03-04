(in-package :cl-css)

;;;;;;;;;; helpers
(defun render-selector (s)
  (if (symbolp s) (string-downcase (symbol-name s)) s))

(defun format-directive (d) 
  (format nil "~a { ~{~(~a: ~a;~) ~}}~%" (render-selector (car d)) (cdr d)))

;;;;;;;;;; generators
(defun inline-css (directive) (format nil "~(~{ ~a: ~a; ~}~)" directive))

(defun css (directives)
  (reduce (lambda (a b) (concatenate 'string a (format-directive b)))
	      (cons "" directives)))

(defun compile-css (file-path directives)
  (ensure-directories-exist file-path)
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (format stream (css directives))))
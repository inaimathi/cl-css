(in-package :cl-css)

(defmethod format-selector ((s symbol)) (symbol->d-string s))
(defmethod format-selector ((s string)) s)

(defmethod format-declaration (property (value string))
  (concatenate 'string (symbol->d-string property) ": " value ";"))
(defmethod format-declaration (property (value symbol))
  (concatenate 'string (symbol->d-string property) ": " (symbol->d-string value) ";"))
(defmethod format-declaration (property (value list))
  (concatenate 'string (symbol->d-string property) " { " (format-declarations-list value) "}"))
(defmethod format-declaration ((property list) (v null))
  (declare (ignore v))
  (format-declarations-list property ))

(defun format-declarations-list (list-of-declarations)
  (apply #'concatenate 
	 'string 
	 (loop for (prop val) on list-of-declarations by #'cddr
	       collect (format-declaration prop val)
	       collect " ")))

(defun format-rule (selector declarations)
  (concatenate 'string (format-selector selector) 
	       " { " (format-declarations-list declarations) "}"))

;;;;;;;;;; generators
(defun inline-css (rule) (format-declarations-list rule))

(defun css (rules)
  (apply #'concatenate 
	 'string 
	 (loop for r in rules
	       collect (format-rule (car r) (cdr r))
	       collect (list #\Newline))))

(defun compile-css (file-path directives)
  (ensure-directories-exist file-path)
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (format stream (css directives))))
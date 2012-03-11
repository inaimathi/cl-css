(in-package :cl-css)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun symbol->d-string (a-symbol)
  "Returns the downcased symbol-name of the given symbol"
  (string-downcase (symbol-name a-symbol)))

(defun prefix-symbol (prefix symbol)
  (intern (format nil "~a~a" prefix symbol) :keyword))

(defmethod %-or-word ((val number)) (concatenate 'string (write-to-string val) "%"))
(defmethod %-or-word ((val symbol)) (symbol-name val))
(defmethod %-or-word ((val string)) val)
(defmethod %-or-word ((val null)) nil)

(defmacro split-directive (directive-name value &optional (prefix-list '(-ms- -o- -webkit- -moz-)))
  (with-gensyms (val)
    `(let ((,val ,value)) 
       (list ,directive-name ,val
	     ,@(loop 
		 for p in prefix-list
		 collect (prefix-symbol p directive-name)
		 collect val)))))
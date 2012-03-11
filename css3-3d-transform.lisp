(in-package :cl-css)

(defun perspective (n)
  (split-directive :perspective n (-webkit-)))

(defun perspective-origin (x y)
  (split-directive :perspective-origin 
      (concatenate 'string (%-or-word x) " " (%-or-word y)) (-webkit-)))

(defun backface-visibility (visible/hidden)
  (split-directive :backface-visibility visible/hidden (-webkit- -moz-)))

(defun transform-style (flat/preserve-3d)
  (split-directive :transform-style flat/preserve-3d (-webkit-)))

(defun matrix3d (&rest 16-numbers)
  (split-directive :transform (format nil "matrix3d(~{~a~^,~})" 16-numbers) (-webkit- -moz-)))

(defun translate3d (x y z &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-directive :transform 
      (format nil "translate3d(~a~a, ~a~a, ~a~a)" x units y units z units) (-webkit- -moz-)))

(defun scale3d (scale-x &optional (scale-y scale-x) (scale-z scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-directive :transform (format nil "scale3d(~a,~a,~a)" scale-x scale-y scale-z) (-webkit- -moz-)))

(defun rotate3d (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-directive :transform (format nil "rotate3d(~adeg)" degrees) (-webkit- -moz-)))
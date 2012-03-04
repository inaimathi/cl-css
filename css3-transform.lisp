(in-package :cl-css)

(defun split-transform-directive (d)
  `(:transform ,d
    :-ms-transform ,d
    :-webkit-transform ,d
    :-o-transform ,d
    :-moz-transform ,d))

(defun transform-origin (x y &optional z)
  "Takes x, y, z percentages, returns a cross-browser CSS3 transform-origin directive"
  (split-transform-directive
   (format nil "~a% ~a%~@[ ~a%~]" x y z)))

(defun rotate (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-transform-directive 
   (format nil "rotate(~adeg)" degrees)))

(defun scale (scale-x &optional (scale-y scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-transform-directive
   (format nil "scale(~a,~a)" scale-x scale-y)))

(defun skew (x-deg y-deg)
  (split-transform-directive
   (format nil "skew(~adeg, ~adeg)" x-deg y-deg)))

(defun translate (x y &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-transform-directive
   (format nil "translate(~a~a, ~a~a)"
	   x units y units)))

(defun matrix (a b c d e f)
  "Takes six numbers and uses them to build a CSS3 transformation matrix directive"
  (split-transform-directive
   (format nil "matrix(~{~a~^,~})" (list a b c d e f))))
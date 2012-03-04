(defpackage :cl-css
  (:nicknames "MINI-CSS")
  (:use :cl)
  (:export :css :inline-css :compile-css
	   :transform-origin :rotate :scale :skew :translate :matrix)
  (:documentation "A non-validating, inline CSS generator for common lisp"))
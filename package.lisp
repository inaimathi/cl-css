(defpackage :cl-css
  (:nicknames "MINI-CSS")
  (:use :cl)
  (:export 
   :css :inline-css :compile-css 
   :% :em :px
   :transform-origin :rotate :scale :skew :translate :matrix
   :perspective :perspective-origin :backface-visibility :transform-style :matrix3d :translate3d :scale3d :rotate3d
   :keyframes :animation :transition)
  (:documentation "A non-validating, inline CSS generator for common lisp"))

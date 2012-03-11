(in-package :cl-css)

(defun keyframes (animation-name &rest keyframes)
  (flet ((sel (browser-type) 
	   (list (format nil "@~@[~(~a~)~]keyframes ~a" browser-type (format-selector animation-name)) 
		 keyframes)))
    `(,(sel nil) ,(sel :-moz-) ,(sel :-webkit-))))

(defun animation (name &key (duration 0) (timing-function :linear) (delay 0) (iteration-count 1) (direction :normal) (play-state :running))
  (split-directive 
      :animation 
      (format nil "~a ~as ~a ~as ~a ~a ~a"
	      name duration timing-function delay iteration-count direction play-state)
      (-webkit- -moz-)))

(defun transition (property &key (duration 0) (timing-function :ease) (delay 0))
  (split-directive
      :transition
      (format nil "~a ~as ~a ~as" property duration timing-function delay)
      (-webkit- -moz- -o-)))
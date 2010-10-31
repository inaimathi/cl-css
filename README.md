CL-CSS
======

This is a dead-simple, non validating, inline CSS generator for Common Lisp. Its goals are axiomatic syntax, simple implementation to support portability, and boilerplate reduction in CSS. Execution efficiency, and CSS validation are non-goals at the moment.

### Basic Usage

Use the `css` function to convert a list of directives into a CSS string for output. A directive is a CSS selector, followed by a list of properties.

	> (css '((body :margin 5px :padding 0px)))
	
	"body { margin: 5px; padding: 0px; }
	"

### Compound Selectors and Properties

Both cases are handled as strings.

	> (css '(("body h1 .header" :margin "5px 0px 0px 5px")))

	"body h1 .header { margin: 5px 0px 0px 5px; }
	"

### Boilerplate reduction

You can stitch things into the directives you pass to `css`. Whether variables

	> (defvar slim-box '(:margin 0px :padding 0px :border "1px solid #f00"))

	SLIM-BOX

	> (css `((.sidebar ,@slim-box :background-color \#0f0)
	         (.float-box ,@slim-box :background-color \#00f :font-weight bold)))

	".sidebar { margin: 0px; padding: 0px; border: 1px solid #f00; background-color: #0f0; }
	.float-box { margin: 0px; padding: 0px; border: 1px solid #f00; background-color: #00f; font-weight: bold; }
	"
or functions

	> (defun sm-box (&optional color) 
	         `(:margin 0px :padding 0px :border "1px solid #f00" 
	           :background-color ,(format nil "2px solid ~a" (or color "#0f0"))))
	
	SM-BOX
	
	> (css `((.sidebar ,@(sm-box))
	         (.float-box ,@(sm-box \#00f) :font-weight bold)))
		 
	".sidebar { margin: 0px; padding: 0px; border: 1px solid #f00; background-color: #0f0; }
	.float-box { margin: 0px; padding: 0px; border: 1px solid #f00; background-color: #00f; font-weight: bold; }
	"

### Noted bad stuff or non-goals

+ The current version of cl-css does all work at runtime. Ideally, there would be a cursory parse during compile time to check if anything can be nailed down at that point. 
+ No validation is done. If you provide the incorrect number of arguments, you'll get an error, but if you try something like `(css '(body :magrin 5px))`, you'll get no help.

### Trivia

As of this writing, the license for this module outweighs the actual program in terms of line count.

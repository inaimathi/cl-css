CL-CSS
======

This is a dead-simple, non validating, inline CSS generator for Common Lisp. Its goals are axiomatic syntax, simple implementation to support portability, and boilerplate reduction in CSS. Execution efficiency, and CSS validation are non-goals at the moment.

### Basic Usage

Use the `css` function to convert a list of directives into a CSS string for output. A directive is a CSS selector, followed by a list of properties.

	> (css '((body :margin 5px :padding 0px)))
	
	"body { margin: 5px; padding: 0px; }
	"

### Inline Usage

Use the `inline-css` function to convert a single, selector-free directive into contents appropriate for a `style` property

        > (inline-css '(:margin 5px :padding 0px))
	
	"margin: 5px; padding: 0px;"

### Compiled Usage

A function called `compile-css` is provided that generates a file based on your cl-css markup.

	> (compile-css "/home/user-name/page-style.css" '((body :margin 5px :padding 0px)))
	
	NIL
	
There will now be a file at `/home/user-name/` named `page-style.css` that contains

	"body { margin: 5px; padding: 0px; }
	"

You can reference this flat file from your web-app (or host it from a non-lisp server like Apache or nginx) to save some time in generating your styles. If the directory you compile to doesn't exist, `compile-css` will attempt to create it before compiling the file.

### Compound Selectors and Properties

Both cases are handled as strings.

	> (css '(("body h1 .header" :margin "5px 0px 0px 5px")))

	"body h1 .header { margin: 5px 0px 0px 5px; }
	"

### Case (in)Sensitivity

All output is downcased, with the exception of selectors and values specified as strings.

	> (css `((.someCrazyClassName :width 30PX :font-family :Helvetica)))
	
	".somecrazyclassname { width: 30px; font-family: helvetica; }
	"
	
	> (css `((".someCrazyClassName" :width 30px :font-family "Helvetica")))
	
	".someCrazyClassName { width: 30px; font-family: Helvetica }
	"

### Nested Terms

Sublists are interpreted as nested CSS rules. This is useful in places like `@media` directives.

	> (css '(("@media screen and (max-width: 720px)" ("body" :padding 1em))))
	
	"@media screen and (max-width: 720px) { body { padding: 1em; } }
	"
	
	> (css '(("@media screen and (max-width: 720px)" 
		  (body :padding 1em :margin 2em) 
		  (.header :background-color :blue) 
		  :font-family "Helvetica")
		 (body :padding 10px :margin 15px)))
		 
	"@media screen and (max-width: 720px) { body { padding: 1em; margin: 2em; } .header { background-color: blue; } font-family: Helvetica; }
	body { padding: 10px; margin: 15px; }
	"

### Boilerplate reduction

You can stitch things into the directives you pass to `css` to eliminate repetition. Whether variables

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

### CSS3 Cross-Browser Abstractions

The CSS3 transform property is handled very slightly differently in each of the major browsers. `cl-css` provides a number of functions to make this easier. For example

        > (css `((.crazy-box ,@(translate 35 35))))
	
	".crazy-box { transform: translate(35px, 35px); -ms-transform: translate(35px, 35px); -webkit-transform: translate(35px, 35px); -o-transform: translate(35px, 35px); -moz-transform: translate(35px, 35px); }
        "
A number of similar functions are provided out of the box for other `transform`, `3d-transform`, `animation` and `transition` directive options. These include `transform-origin`, `rotate`, `scale`, `skew` and `matrix`, `perspective`, `perspective-origin`, `keyframes`, `animation` and `transition`. The list is not exhaustive, even in terms of new CSS3 selectors, let alone abstracting directives for older browsers, but it should be fairly straightforward to define more of your own (feel free to send appropriate patches if you do end up using other directives).


### Noted bad stuff or non-goals

+ No validation is done. If you provide the incorrect number of arguments, you'll get an error, but if you try something like `(css '((body :magrin 5px)))` or `(inlne-css '(:margin 5 :padding 5))`, you'll get no help.

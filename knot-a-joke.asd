;;;; knot-a-joke.asd

(asdf:defsystem #:knot-a-joke
  :description "Assorted collection of tools related to knot theory"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:iterate #:cl-itertools #:esrap-liquid #:defmacro-enhance #:cl-interpol)
  :components ((:file "package")
	       (:file "parsing-macro")
	       (:file "parsing")
               (:file "knot-a-joke")
	       (:static-file "rep-graph-2.txt")))


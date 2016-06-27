;;;; knot-a-joke.asd

(asdf:defsystem #:knot-a-joke
  :description "Assorted collection of tools related to knot theory"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "knot-a-joke")))


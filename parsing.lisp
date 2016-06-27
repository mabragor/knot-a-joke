
(in-package #:knot-a-joke)

;; OK, let's learn how to parse a representation graph
(define-rg-rule digit ()
  (character-ranges (#\0 #\9)))

(define-rg-rule posint ()
  (parse-integer (text (postimes digit))))

(define-rg-rule whitespace ()
  (postimes (|| #\space #\tab)))

(define-rg-rule start-layer ()
  (v "layer :") (? whitespace) (cap int posint) (? whitespace) (v #\newline)
  (list :start-layer (recap int)))

(define-rg-rule end-layer ()
  (v "end layer :") (? whitespace) (cap int posint) (? whitespace) (v #\newline)
  (list :end-layer (recap int)))

(define-rg-rule any-line ()
  (text (times (!! #\newline)) (v #\newline)))

(define-rg-rule complete-layer ()
  (cap start start-layer)
  (cap the-meat (times (progn-v (! (|| start-layer end-layer))
				any-line)))
  (cap end end-layer)
  (if (equal (cadr (recap start))
	     (cadr (recap end)))
      (list :layer (cadr (recap start)) (rg-parse '(times data-line) (text (recap the-meat))))
      (fail-parse "Not a validly formed layer")))

(define-rg-rule list-of-posints ()
  (let* ((first (v posint))
	 (rest (times (progn-v #\, (? whitespace) posint))))
    (cons first rest)))
    
(define-rg-rule braced-of-posints ()
  (progm #\{ list-of-posints #\}))

(define-rg-rule graph-edge ()
  (cap to braced-of-posints)
  (? whitespace) (v "->") (? whitespace)
  (cap mult posint)
  (list (recap to) (recap mult)))

(define-rg-rule data-line ()
  (cap from braced-of-posints)
  (? whitespace) (v #\:) (? whitespace)
  (cap tos (times (prog1-v graph-edge #\, (? whitespace))))
  (v #\newline)
  (cons (recap from) (recap tos)))

(defun parse-rg-file (fname)
  (with-open-file (stream fname)
    (rg-parse-stream '(times complete-layer) stream :junk-allowed t)))

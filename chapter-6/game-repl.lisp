(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
  (let ((allowed-commands '(look walk pickup inventory)))
    (if (member (car sexp) allowed-commands)
	(eval sexp)
        '(I do not know this command))))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil lit)))))))

(defun game-print (list)
  "game print documentation"
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string list))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
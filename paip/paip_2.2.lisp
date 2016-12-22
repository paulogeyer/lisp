(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (let ((gender (Gender)))
			(append (Article gender) (Noun gender))))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Gender ()      (car (one-of '(male female))))
(defun Article (g)     (one-of (if (eql g 'female)
				   '(a uma)
				   '(o um))))
(defun Noun (g)        (one-of (if (eql g 'female)
				   '(bola mulher mesa)
				   '(homem menino computador))))
(defun Verb ()        (one-of '(chutou sentiu tomou viu cheirou)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

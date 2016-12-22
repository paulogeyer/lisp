(defpackage :on-lisp
  (:use :cl))

(in-package :on-lisp)

(defun behave (animal)
  (case animal
    (dog (wag-tail)
         (bark))
    (rat (scurry)
         (squeak))
    (cat (rub-legs)
         (scratch-carpet))))

;; pg 16
(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(let ((y 5)) (scope-test 3))

;; pg 18
(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

(let ((counter 0))
  (defun new-id ()   (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;; pg 19
(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

;; pg 20
(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

;; (setq cities (make-dbms '((boston . us) (paris . france))))

(defun lookup (key db)
  (funcall (car db) key))

;; pg 22
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))

;; (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

(defun our-length (lst)
  (if (null lst)
      0
      (1+ (out-lenght (cdr lst)))))

;; pg 23

(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (if (funcall fn (car lst))
          (car lst)
          (our-find-if (cdr lst)))))

(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

;; pg 24
(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

;; pg 29
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

;; pg 30
(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;; pg 35
(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))

(let ((x 0))
  (defun total (y)
    (incf x y)))

;; pg 37
(defun exclaim (expression)
  (append expression '(oh my)))

(defun exclaim (expression)
  (append expression (list 'oh 'my)))

;; pg 45
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; pg 62
(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))

(defun join (&rest args)
  (apply (joiner (car args)) args))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;; pg 63
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

;; pg 64
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(def! #'remove-if #'delete-if)

(setq lst '(1 2 3 4 5))

(delete-if #'oddp lst)

(funcall (! #'remove-if) #'oddp lst)

;; pg 65
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(setq slowid (memoize #'(lambda (x) (sleep 5) x)))

(defun factorial (n)
  (if (< n 1)
      1
      (* n (factorial (1- n)))))

;; pg 66
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;; pg 69
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0)
         '(1 2 3))

(funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
         '(1 2 3 '(t)))

;; pg 70

;; copy-list
(lrec #'(lambda (x f) (cons x (funcall x))))

;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

;; pg 72
(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))

(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))

(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten (cdr tree))))))

;; pg 73
(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))

(rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5))

;; pg 74
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

;; our-copy-tree
(ttrav #'cons)

;; count-leaves
(ttrav #'(lambda (l r) (+ 1 (or r 1))) 1)

;; flatten
(ttrav #'nconc #'mklist)

;; pg 75
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                      #'(lambda ()
                          (self (car tree)))
                      #'(lambda ()
                          (if (cdr tree)
                              (self (cdr tree))))))))
    #'self))

;; flatten version using trec
(trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
      #'mklist)

;; now we can also express rfind-if for e.g. oddp as:
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
               #'(lambda (tree) (and (oddp tree) tree)))o
(funcall *
         '(2 8 6 4 3 4 5 6))

;; pg 78
(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes      yes
                   :no       no)))

(defun defnodes ()
  (defnode 'people "Is the person a man?" 'male 'female)
  (defnode 'male "Is he living?" 'liveman 'deadman)
  (defnode 'deadman "Was he American?" 'us 'them)
  (defnode 'us "Is he on a coin?" 'coin 'cidence)
  (defnode 'coin "Is the coin a penny?" 'penny 'coins)
  (defnode 'penny 'lincoln))

;; pg 79

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> ") (node-contents n))
          (case (read)
            (yes (run-node (node-yes n)))
            (t   (run-node (node-no n))))
          (t (node-contents n)))))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>> " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t   (funcall (gethash no *nodes*)))))
            #'(lambda () conts))))

(setq *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (yes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn  (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))

;; pg 83
(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil! (var)
  `(setq ,var nil))

;; pg 86

;; with backquote (`)
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;; without backquote
(defmacro nif (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))

;; pg 87
(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

;; pg 88

(defun greet (name)
  `(hello ,name))

(member x choices :test #'eq)

(defmacro memq (obj list)
  `(member ,obj ,list :test #'eq))

;; pg 92
(defmacro while (test &body body)
  `(do ()
       ((not, test))
     ,@body))

(pprint (macroexpand '(while (able) (laught))))

(pprint (macroexpand-1 '(while (able) (laught))))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; pg 94
(dolist (x '(a b c))
  (print x))

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro our-expander (name) `(get ,name 'expandar))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,parms (cdr ,g)
                     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))

(let ((op 'setq))
  (defmacro our-setq (var val)
    (list op var val)))

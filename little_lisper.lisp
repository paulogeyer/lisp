(princ 'foo)

(defun add1
    (let ((f '+))
      (lambda (x) (f x 1))))

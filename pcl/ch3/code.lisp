(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; (make-cd "Roses" "Kathy Mattea" 7 t)

(defun add-record (cd) (push cd *db*))

;; (add-record (make-cd "One Hot Minute" "Red Hot Chili Peppers" 8 t))
;; (add-record (make-cd "Homework" "Daft Punk" 7 t))
;; (add-record (make-cd "Human After All" "Daft Punk" 8 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; (save-db "/home/paulo/code/lisp/pcl/ch3/cds.db")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; (load-db "/home/paulo/code/lisp/pcl/ch3/cds.db")

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; (defun make-comparison-expr (field value)
;;   (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (cd)
;;       (and
;;        (if title    (equal (getf cd :title)  title)  t)
;;        (if artist   (equal (getf cd :artist) artist) t)
;;        (if rating   (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-row (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; (defmacro backwards (expr) (reverse expr))
;; (backwards "hello, world" t format)

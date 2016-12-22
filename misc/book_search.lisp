(ql:quickload '(dexador
                jsown))

;; Example ISBNs
;; On Lisp (Paul Graham)
;; 0130305529

;; (defvar url "https://openlibrary.org/api/books?bibkeys=ISBN:0201558025,LCCN:93005405&format=json")
;; (setq page (dexador:get url))
;; (setq doc (jsown:parse page))

(defun make-url (isbn)
  (concatenate 'string 
               "https://openlibrary.org/api/books?bibkeys=ISBN:"
               (format nil "~a" isbn)
               ",LCCN:93005405&format=json&jscmd=data"))

(defun get-json (isbn)
  (let* ((url (make-url isbn))
         (page (dexador:get url))
         (doc (jsown:parse page)))
    doc))

;; (ql:quickload 'drakma)
;; (ql:quickload 'cxml)
(ql:quickload 'dexador)
(ql:quickload 'closure-html)
(ql:quickload 'css-selectors)

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun find-all-gfs (class-name)
  (remove-duplicates
   (mapcan (lambda (class)
             (copy-list (sb-mop:specializer-direct-generic-functions class)))
           (sb-mop:compute-class-precedence-list (find-class class-name)))))

(defvar *base-url* "http://www.angeloni.com.br")
(defvar *url* "http://www.angeloni.com.br/super/index")
;; (setf *url* "http://www.angeloni.com.br/super/index")

(defvar *CATEGORIES* (make-hash-table))
(defvar *PRODUCTS* (make-hash-table))

(defun save-category (id name href products)
  (setf (gethash id *CATEGORIES*)
        (list :name name
              :href href
              :products products)))

(defun get-and-save-all-categories ()
  (mapcar #'(lambda (c)
              (save-category (getf c :id)
                             (getf c :name)
                             (getf c :href)
                             (get-and-save-all-products (getf c :id))))
          (get-categories (parse-page (get-page *url*)))))

(defun save-product (id category-id name price href)
  (setf (gethash id *PRODUCTS*)
        (list :name name
              :category-id category-id
              :price price
              :href href)))

(defun get-and-save-all-products (category-id)
  (let ((doc
         (parse-page (get-page (generate-category-products-url1 category-id)))))
    (mapcar #'(lambda (entry)
                (save-product (getf entry :id)
                              category-id
                              (getf entry :name)
                              (getf entry :price)
                              (concatenate 'string
                                           *base-url*
                                           (getf entry :href)))
                (getf entry :id))
            (parse-category-list doc))))

(defun get-page (url)
  (dex:get url))

(defun parse-page (page)
  (chtml:parse page (cxml-dom:make-dom-builder)))

(defun find-categories-elts (doc)
  (css:query "ul#navRoot li a" doc))

(defun get-categories (doc)
  (labels ((get-id (s)
             (parse-integer 
              (car (last (split-sequence:split-sequence #\= s)))
              :junk-allowed t)))
    (mapcar #'(lambda (n)
                (let* ((name (dom:node-value (dom:first-child n)))
                       (href (dom:get-attribute n "href"))
                       (id (get-id href)))
                  (list :id id
                        :name name
                        :href href)))
            (find-categories-elts doc))))

(defun generate-category-products-url (category_id)
  (format nil "http://www.angeloni.com.br/super/ofertas?grupo=~a&itemsPerPage=999" category_id))

(defun generate-category-products-url1 (category_id)
  (format nil "http://www.angeloni.com.br/super/produtos?grupo=~a&ordem=0&itemsPerPage=999&page=1" category_id))

(defun parse-category-list (doc)
  (remove-if #'(lambda (x) (equal x 'nil))
             (mapcar #'parse-product-list-entry
                     (css:query "ul.lstProd li" doc))))

(defun parse-product-list-entry (doc)
  (labels ((parse-name (name-elt)
             (dom:node-value (dom:first-child name-elt)))
           (parse-href (name-elt)
             (dom:get-attribute name-elt "href"))
           (parse-id (name-elt)
             (parse-integer (car (last (split-sequence:split-sequence
                                        #\=
                                        (dom:get-attribute name-elt "href"))))))
           (parse-price (price-elt)
             (float (+ (parse-integer (clean-string (dom:node-value (dom:first-child (car (css:query ".boxPrice .price a" doc))))) :junk-allowed t)
                       (/ (parse-integer (dom:node-value (dom:first-child (car (css:query ".boxPrice .price a span" doc))))) 100)))))
    (let ((name-elt (car (css:query ".descr a" doc))))
      (if name-elt
          (list :id (parse-id name-elt)
                :name (parse-name name-elt)
                :price (parse-price doc)
                :href (parse-href name-elt))
          'nil))))

(defun parse-product-page (doc)
  (labels ((get-name (doc)
             (clean-string (dom:node-value (DOM:FIRST-CHILD (car (css:query ".boxIn #boxTtl h1" doc))))))
           (get-price (doc)
             (+
              (parse-integer (dom:node-value
                              (dom:first-child
                               (car (css:query "span.valorPrice" doc)))))
              (/ (parse-integer (subseq (dom:node-value (dom:first-child (car (css:query "span.valorPrice .decimais" doc)))) 1 3)) 100)
                          )))
    (list :name (get-name doc)
          :price (float (get-price doc)))))

;; get attribute
;; (DOM:get-attribute (elt categories 1) "href" )

;; funções que usam os dados coletados pelas funções acima
(defun category-product-count (id)
  (length (getf (gethash id *CATEGORIES*) :products)))

(defun product-price (id)
  (getf (gethash id *PRODUCTS*) :price))

(defun category-price-average (id)
  (if (not (equal (category-product-count id) 0))
      (/ (apply #'+ (loop for i in (getf
                                    (gethash id *CATEGORIES*)
                                    :products)
                       collect (product-price i)))
         (category-product-count id))
      0))

(defun print-product (id)
  (let* ((product (gethash id *PRODUCTS*))
         (product-name (getf product :name))
         (product-price (getf product :price))
         (product-category (getf product :category-id)))
    (format t "[Product]~%Name: ~a~%Price: ~a~%Category: ~a~%"
            product-name
            product-price
            product-category)
    (print-category product-category)))

(defun print-category (id)
  (format t "ID: ~a~%Category: ~a~%Total products: ~a~%Price average: ~a~%"
          id
          (getf (gethash id *CATEGORIES*) :name)
          (category-product-count id)
          (category-price-average id)))

(defun print-all-categories ()
  (mapcar #'print-category
          (alexandria:hash-table-keys *CATEGORIES*)))



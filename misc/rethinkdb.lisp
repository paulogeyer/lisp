(ql:quickload 'cl-rethinkdb)
(ql:quickload 'cl-async)
(ql:quickload 'blackbird)

(use-package :blackbird)

;; (setq promise (alet* ((sock (connect "127.0.0.1" 28015))
;;                       (query (r (:get (:table "users") 12)))  ; get user id 12
;;                       (value (run sock query)))
;;                 (format t "My user is: ~s~%" value)
;;                 (disconnect sock)))


;; (setq sock (cl-rethinkdb:connect "127.0.0.1" 28015 :db "magnets"))

(defun count-magnets ()
  (with-promise (resolve reject)
    (alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015
                                        :db "magnets"))
            (query (cl-rethinkdb:r (:count (:table "magnets"))))
            (value (cl-rethinkdb:run sock query)))
      (format t "Count: ~a~%" value)
      (cl-rethinkdb:disconnect sock))))

(defun add-magnet (magnet)
  (with-promise (resolve reject)
    (alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015
                                        :db "magnets"))
            (query (cl-rethinkdb:r (:insert (:table "magnets") magnet)))
            (value (cl-rethinkdb:run sock query)))
      (format t "Added: ~a~%" value)
      (cl-rethinkdb:disconnect sock))))

(defmacro filter-magnets (xt)
  `(as:with-event-loop ()
    (alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015
                                        :db "magnets"))
            (query (cl-rethinkdb:r (:filter (:table "magnets") '(("xt" . ,xt)))))
            (cursor (cl-rethinkdb:run sock query)))
      (wait (cl-rethinkdb:each sock cursor
                  (lambda (x)
                    (format t "~a" (alexandria:hash-table-keys x))))
        (wait (cl-rethinkdb:stop sock cursor)
          (cl-rethinkdb:disconnect sock))))))

(as:with-event-loop ()
  (with-promise (resolve reject)
    (alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015
                                        :db "magnets"))
            (query (cl-rethinkdb:r (:count (:table "magnets"))))
            (value (cl-rethinkdb:run sock query)))
      (princ value)
      (cl-rethinkdb:disconnect sock))))


(ql:quickload '(cl-who
                hunchentoot
                dexador
                closure-html
                cxml
                css-selectors
                lquery
                plump
                quri
                cl-ppcre
                cl-rethinkdb))

(defpackage :magnets
  (:use :cl :cl-who :hunchentoot))

(in-package :magnets)

(defvar *server* nil)
(defvar *port* 3000)
(defvar *magnets* '())

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defclass magnet-url ()
  ((dn :reader dn
       :initarg :dn)
   (xt :reader xt
       :initarg :xt)
   (trackers :reader trackers
             :initarg :trackers)))

(defmethod print-object ((object magnet-url) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (dn xt trackers) object
      (format stream
              "dn: ~a, xt: ~a, trackers: ~{~a~^, ~}"
              dn xt trackers))))

(defun magnet-to-plist (magnet)
  (list `("dn" . ,(dn magnet))
        `("xt" . ,(xt magnet))
        `("trackers" . ,(trackers magnet))))

(defun magnets ()
  *magnets*)

(defun add-magnet (magnet)
  (let* ((magnet-obj (quri:uri-query-params (quri:uri magnet)))]
         (magnet-plist (mapcar #'(lambda (x) (list (intern (car x) "KEYWORD") (cdr x))) magnet-obj))
         (dn (cadr (assoc :|dn| magnet-plist)))
         (xt (cadr (assoc :|xt| magnet-plist)))
         (trackers (mapcar #'(lambda (x)
                               (clean-string (cadr x)))
                           (remove-if-not
                            #'(lambda (x)
                                (eq :|tr| (car x))) magnet-plist))))
    ;;magnet-plist))
    (push (make-instance 'magnet-url
                         :dn dn
                         :xt xt
                         :trackers trackers)
          *magnets*)))

(defun get-magnet-by-xt (x)
  (car (remove-if-not #'(lambda (m)
                          (eq x (xt m)))
                      *magnets*)))

(defun parse-magnet-from-string (magnet)
  (mapcar #'(lambda (x)
              (split-sequence:split-sequence
               #\=
               x))
          (split-sequence:split-sequence #\& (subseq magnet 8))))

(defmacro standard-page ((&key title script) &rest body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             ,(when script
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "header" ; Retro games header
                   (:img :src "/logo.jpg"
                         :alt "Magnets"
                         :class "logo")
                   (:span :class "strapline"
                          "How do they work?"))
             ,@body))))

(defun start-server (&key port)
  (if port
      (setf *port* port))
  (setq *server*
        (start (make-instance 'easy-acceptor :port *port*))))

(defun stop-server ()
  (stop *server*))

(defun restart-server ()
  (and (stop-server)
       (start-server)))

(define-easy-handler (magnets :uri "/") ()
  (standard-page (:title "Magnets! How do they work?")
    (:h1 "Last magnets found")
    (:ol
     (dolist (magnet-li *magnets*)
       (htm
        (:li (fmt "~a" (dn magnet-li))))))))

     ;; (:li "Magnet 1")
     ;; (:li "Magnet 2"))))

;; Piratebay recent torrents
;; https://thepiratebay.org/recent

(defun remove-if-nil (lst)
  (remove-if #'(lambda (x) (eql x nil))
             lst))

(defun get-page ()
  (dex:get "https://thepiratebay.org/recent"))

(defun parse-page (page)
  (plump:parse page))

(defun is-torrent? (tr-entry)
  (= 1 (length (lquery:$ tr-entry ".detName a"))))

(defun parse-magnet-link (entry)
  (lquery:$1 entry "a" (eq 3) (attr "href")))

(defun parse-magnet-name (entry)
  (lquery:$1 entry ".detName a" (text)))

(defun magnet-list (doc)
  (remove-if-not #'is-torrent?
                 (lquery:$ doc "table#searchResult tr")))

(defun save-magnet-list (magnets)
  (map 'vector #'save-magnet magnets))

(defun save-magnet (magnet)
  (let ((magnet-link (parse-magnet-link magnet)))
    (format t "magnet ~a~%" magnet-link)
    (add-magnet (parse-magnet-link magnet))))

(defun fetch-and-save-magnets ()
  (save-magnet-list (magnet-list (parse-page (get-page)))))

(defun count-magnets ()
  (as:with-event-loop ()
    (blackbird:alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015 :db "magnets"))
                      (query (cl-rethinkdb:r (:count (:table "magnets"))))
                      (cursor (cl-rethinkdb:run sock query)))
      (format t "Count: ~a~%" cursor)
      (cl-rethinkdb:disconnect sock))))

(defmacro rethink-add-magnet (magnet)
  `(as:with-event-loop ()
     (blackbird:alet* ((sock (cl-rethinkdb:connect "127.0.0.1" 28015 :db "magnets"))
                       (query (cl-rethinkdb:r (:insert (:table "magnets") ,magnet)))
                       (value (cl-rethinkdb:run sock query)))
       (format t "Added: ~a~%" value)
       (cl-rethinkdb:disconnect sock))))

;; Regex to parse size and author from each torrent
;; eg: Size 5.83 GiB, ULed by SudoSu
;; returns 5.83 and SudoSu
;; (cl-ppcre:scan-to-strings
;;  (cl-ppcre:parse-string "Size (.+) GiB, ULed by (.+)")
;;  str)


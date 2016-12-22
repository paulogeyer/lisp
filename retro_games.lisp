(ql:quickload '(cl-who
                hunchentoot
                parenscript))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)
;; (defclass game ()
;;   ((name :initarg :name)
;;    (votes :initform 0)))

(defvar *games* '())

;; cl-who set to html5
(setf (html-mode) :html5)

(defclass game ()
  ((name :reader name
         :initarg :name)
   (votes :accessor votes
          :initform 0)))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defun game-from-name (name)
  (find name *games* :test #'string-equal
        :key #'name))

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name ~s with ~d votes" name votes))))

;; (setf (html-mode) :html5)

;; (with-html-output (*standard-output* nil :indent t)
;;   (:html
;;    (:head
;;     (:title "Test page"))
;;    (:body
;;     (:p "CL-WHO is really easy to use"))))

;; (with-html-output (*standard-output* nil :prologue t :indent t)
;;   (:html
;;    (:head
;;     (:title "Test page"))
;;    (:body
;;     (:p "CL-WHO is really easy to use"))))

(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (:prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css")
             ,(when script
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "header" ; Retro games header
                   (:img :src "/logo.jpg"
                         :alt "Commodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favourite Retro Game"))
             ,@body))))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

;; (push (create-prefix-dispatcher "/retro-games.htm"
;;                                 'retro-games)
;;       *dispatch-table*)

;; (defun retro-games ()
;;   (standard-page (:title "Retro Games")
;;     '((:h1 "Top Retro Games")
;;       (:p "We'll write the code later..."))))

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart" ; Used for CSS styling of the links.
          (:ol
           (dolist (game (games))
             (htm
              (:li (:a :href (format nil "vote?name=~a"
                                     (url-encode ; avoid injection attacks
                                      (name game))) "Vote!")
                   (fmt "~A with ~d votes" (escape-string (name game))
                        (votes game)))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                         :script (ps ; client side validation
                                   (defvar add-form nil)
                                   (defun validate-game-name (evt)
                                     (when (= (@ add-form name value) "")
                                       (chain evt (prevent-default))
                                       (alert "Please enter a name.")))
                                   (defun init ()
                                     (setf add-form (chain document
                                                           (get-element-by-id "addform")))
                                     (chain add-form
                                            (add-event-listener "submit"
                                                                validate-game-name false)))
                                   (setf (chain window onload) init)))
    (:h1 "Add a new game to the cart")
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

(defun validate-game-name (evt)
  (when (= (@ add-form name value) "")
    (chain evt (prevent-default))
    (alert "Please enter a name.")))

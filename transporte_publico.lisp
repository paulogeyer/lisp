(ql:quickload '(dexador
                lquery))

(defpackage :tp
  (:use :cl))

(in-package :tp)

(defvar *companies* '())

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline #\|) str))

(defvar base_url "http://www.pmf.sc.gov.br/servicos/index.php?pagina=onibus")

(defun list-companies ()
  (let* ((page (dexador:get base_url))
         (doc (plump:parse page))
         (options-el (remove-if     ; Remove option with value attibute
                      #'(lambda (x) ; equal to "0"
                          (string= "0" (plump:attribute x "value")))
                      (lquery:$ doc "select[name=empresa] option"))))
    (map 'list #'(lambda (o)
                   (list (parse-integer (plump:attribute o "value"))
                         (clean-string (plump:attribute o "title"))))
         options-el)))

(defun parse-linha (linha-el)
  (let* ((text (plump:text linha-el))
         (linha-split (split-sequence:split-sequence #\- text))
         (id (clean-string (car linha-split)))
         (nome (clean-string (nth 1 linha-split))))
    (list id nome)))

(defun list-linhas (empresa)
  (let* ((page (dexador:post base_url
                             :content `(("passoGeral" . "1")
                                        ("passoEmpresa" . "1")
                                        ("empresa" . ,(format
                                                       nil
                                                       "~a"
                                                       empresa))
                                        ("opcao" . "1")
                                        ("linhaNumero" . "")
                                        ("linhaNome" . "")
                                        ("x" . "31")
                                        ("y" . "21"))))
         (doc (let ((plump:*tag-dispatchers* plump:*html-tags*))
                (plump:parse page)))
         (linhas (lquery:$ doc "#resultados_busca ul.listagem li")))
    (map 'vector #'parse-linha linhas)))

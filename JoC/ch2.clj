(ns joy.ch2)

(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(defn sum-down-from [sum x]
  (if (pos? x)
    (recur (+ sum x) (dec x))
    sum))

;; (let [x 2]
;;   `(1 ~x 3))

;; `(1 ~(2 3))

;; unquote
;; (let [x '(2 3)] `(1 ~x))

;; unquote splicing
;; (let [x '(2 3)] `(1 ~@x))

;; auto generated symbol
;; `potion#

(defn throw-catch [f]
  [(try
     (f)
     (catch ArithmeticException e "No dividing by zero!")
     (catch Exception e (str "You are so bad " (.getMessage e)))
     (finally (println "returning...")))])

(defn hello []
  (println "Hello Cleveland!"))

(defn report-ns []
  (str "The current namespace is " *ns*))

(ns joy.req
  (:require clojure.set))

;; (clojure.set/intersection #{1 2 3} #{3 4 5})

(ns joy.req-alias
  (:require [clojure.set :as s]))

;; (s/intersection #{1 2 3} #{3 4 5})

(ns joy.use-ex
  (:require [clojure.string :refer (capitalize)]))

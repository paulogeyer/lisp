(ns user)

;; (let [imadeuapi 3.14159265358979323846264338327950288419716939937M]
;;   (println (class imadeuapi))
;;   imadeuapi)

;; (let [butiatedit 3.14159265358979323846264338327950288419716939937]
;;   (println (class butiatedit))
;;   butiatedit)

;; (def clueless 9)

;; (class clueless)

;; (class (+ clueless 9000000000000000))

;; (class (+ clueless 90000000000000000000))

;; Overflow

;; (+ Long/MAX_VALUE Long/MAX_VALUE)
;; (unchecked-add (Long/MAX_VALUE) (Long/MAX_VALUE))

;; Underflow
;; (float 0.0000000000000000000000000000000000000000000001)
;; 1.0E-440

(let [approx-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours (* 3600 100 10)
      actual-total (double (* hours actual-interval))
      approx-total (double (* hours approx-interval))]
  (- actual-total approx-total))

(class (int (Long/MAX_VALUE)))

(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)

(+ (+ a b) c)
(+ a (+ b c))

(def a (rationalize 1.0e50))
(def b (rationalize -1.0e50))
(def c (rationalize 17.0e00))

(+ (+ a b) c)
(+ a (+ b c))

(numerator (/ 123 10))
(denominator (/ 123 10))

(def population {:zombies 2700, :humans 9})
(get population :zombies)

(println (/ (get population :zombies)
            (get population :humans))
         "zombies per capita")

(defn pour [lb ub]
  (cond
    (= ub :toujours) (iterate inc lb)
    :else (range lb ub)))

(pour 1 10)
(pour 1 :toujours)

(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y)
   (identical? x y)
   (meta x)
   (meta y)])

(ns where-is)
(def a-symbol 'where-am-i)

(ns user)
(defn best [f xs]
  (reduce #(if (f % %2) % 2) xs))

(best > [1 3 4 2 7 5 3])

;; regular expressions

#"an example pattern"

(re-seq #"\w+" "one-two/three")

(re-seq #"\w*(\w)" "one-two/three")

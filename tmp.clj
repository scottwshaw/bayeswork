(use '(incanter core stats charts))
(use 'bayeswork.core)
(require 'clojure.core.matrix)
(view (histogram (sample-normal 1000)))

(def t (range 0.01 1 0.01))

(print t)
(- (repeat (length t) 1) t)
(pow (- (repeat 1) t) 4)

(defn sub-from-scalar [s v]
  (- (repeat (length v) s) v))

(view (plotbeta 9.5 1.5))

(view (plotprior 1 1))



(use 'clojure.core.matrix.operators)

(def somearray [[1/2 2 3] [4 5 6]])

(* 2 somearray)

(- 2 1)

(pow somearray 2)
(trans somearray)

(use 'bayeswork.core :reload-all)

(view (plot-ptheta 1 100))

;; Excercise 5.6
;; probability given extremely biased prior model (trick coin)
(pdata 0.5 0.5 15 20)
;; probability given strongly peaked prior at 0.5 (fair coin)
(pdata 50 50 15 20)

;; Exercise 5.7
;; Evidence that one heads flip is biased toward heads
(pdata 1 100 1 1)
;; Evidence that one heads flip is biased toward heads
(pdata 100 1 1 1)
;; Bayes factor
(/ (pdata 100 1 1 1) (pdata 1 100 1 1))

(pow (range 0 10 0.1) 2.0)

(minus (range 0.01 1 0.01) 1)
 
(ccmo/-  1)








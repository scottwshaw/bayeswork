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
(trans somearray)





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

(def v [1 2 3 4])

(sqrt (mmult (trans v) v))
(sqrt (sum-of-squares v))
(div v (sum v))
(rest (reductions + 0 (div v (sum v))))
(def ivals (rest (reductions + 0 (div v (sum v)))))
(print ivals)
(let [testval (rand)] (length (take-while #(> testval %) ivals)))

(repeatedly 5 rand)

(reductions + 0 v)

(* 2 somearray)

(- 2 1)

(pow somearray 2)
(trans somearray)


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

;; Exercise 5.8
;; evidence that 8 / 12 flips is a tails coin
(pdata 1 100 8 12)
;; evidence that 8/12 flips is a heads coin
(pdata 100 1 8 12)
;; Bayes factor
(/ (pdata 100 1 8 12) (pdata 1 100 8 12))
;; Predicting the posterior
(view (histogram (beta-posterior-predictions 100 1 8 12)))
;; Exercise 6.1
(view 
 (let [n-intervals 10
       width (/ 1 n-intervals)
       theta (range (/ width 2) (- 1 (/ width 2)) width)
       approx-mass (mult (pdf-beta theta :alpha 8 :beta 4) width)
       p-theta (div approx-mass (sum approx-mass))]
   (bar-chart theta p-theta)))

;; Exercise 6.2
(defn b-grid
  "Chapter 6 function that plots discrete beta stuff"
  [theta p-theta data n-to-plot]
  (let [z (count (filter #(== 1 %) data))
        n (length data)
        p-data-given-theta (map * (pow theta z) (pow (minus 1 theta) (- n z)))
        p-data (sum (mult p-theta p-data-given-theta))]
    (print z n (length p-data-given-theta) (length p-data))))

 (let [shape-theta (concat (num-seq 50 1) (repeat 50 1) (num-seq 1 50) 
                           (num-seq 50 1) (repeat 50 1) (num-seq 1 50))
       p-theta (div shape-theta (sum shape-theta))
       width (/ 1 (length p-theta))
       theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
       data [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
       n-to-plot 99
       [?theta ?prior ?liklihd ?post] (bern-grid theta p-theta data n-to-plot)
       hdi (hdi-of-grid ?post (/ 0.95 3))
       hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                 [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
   (view (bar-chart theta p-theta))
   (view (bar-chart ?theta ?prior))
   (view (bar-chart ?theta ?liklihd))
   (view (add-polygon (xy-plot ?theta ?post) hdi-line)))

;; Exercise 6.3a
 (let [shape-theta (concat (num-seq 50 1) (repeat 50 1) (num-seq 1 50) 
                           (num-seq 50 1) (repeat 50 1) (num-seq 1 50))
       p-theta (div shape-theta (sum shape-theta))
       width (/ 1 (length p-theta))
       theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
       data [1 1 1 0]
       n-to-plot 99
       [?theta ?prior ?likihd ?post] (bern-grid theta p-theta data n-to-plot)
       hdi (hdi-of-grid ?post (/ 0.95 3))
       hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                 [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
   (view (bar-chart theta p-theta))
   (view (bar-chart ?theta ?prior))
   (view (bar-chart ?theta ?likihd))
   (view (add-polygon (xy-plot ?theta ?post) hdi-line)))

;; Exercise 6.3b
 (let [shape-theta (concat (num-seq 50 1) (repeat 50 1) (num-seq 1 50) 
                           (num-seq 50 1) (repeat 50 1) (num-seq 1 50))
       p-theta (div shape-theta (sum shape-theta))
       width (/ 1 (length p-theta))
       theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
       data [1 1 1 0]
       [?theta1 ?prior1 ?likihd1 ?post1] (bern-grid theta p-theta data 99)
       addl-data [1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0]
       [?theta ?prior ?likihd ?post] (bern-grid ?theta1 ?post1 addl-data 101)
       hdi (hdi-of-grid ?post)          ;no need to divide by 3 here take default 0.95
       hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                 [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
   (view (bar-chart ?theta1 ?post1))
   (view (bar-chart ?theta ?prior))
   (view (bar-chart ?theta ?likihd))
   (view (add-polygon (xy-plot ?theta ?post) hdi-line)))

;; Exercise 6.4a
(let [p-theta (repeat 256 1/256)
      width (/ 1 (length p-theta))
      theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
      data (concat (repeat 58 1) (repeat 42 0))
      n-to-plot 100
      [?theta ?prior ?likihd ?post] (bern-grid theta p-theta data n-to-plot)
      hdi (hdi-of-grid ?post (/ 0.95 2.56))
      hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
  (view (add-polygon (xy-plot ?theta ?post) hdi-line)))

;; Exercise 6.4c
 (let [p-theta (repeat 256 1/256)
       width (/ 1 (length p-theta))
       theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
       data (concat (repeat 58 1) (repeat 42 0))
       [?theta1 ?prior1 ?likihd1 ?post1] (bern-grid theta p-theta data 99)
       addl-data (concat (repeat 57 1) (repeat 43 0))
       n-to-plot 256
       [?theta ?prior ?likihd ?post] (bern-grid theta p-theta data n-to-plot)
       [?theta ?prior ?likihd ?post] (bern-grid ?theta1 ?post1 addl-data 101)
       hdi (hdi-of-grid ?post)
       hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                 [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
   (view (add-polygon (xy-plot ?theta ?post) hdi-line)))

;; Exercise 6.5
(let [p-theta (repeat 256 1/256)
      width (/ 1 (length p-theta))
      theta (num-seq (/ width 2) (- 1 (/ width 2)) width)
      data (concat (repeat 58 1) (repeat 42 0))
      n-to-plot 99
      [?theta ?prior ?likihd ?post] (bern-grid theta p-theta data n-to-plot)
      hdi (hdi-of-grid ?post (/ 0.95 3))
      hdi-line [[(nth ?theta (apply min (:indices hdi))) (:height hdi)] 
                [(nth ?theta (apply max (:indices hdi))) (:height hdi)]]]
  (println ?likihd)
  (view (add-polygon (xy-plot ?theta ?post) hdi-line)))


(use 'bayeswork.core :reload-all)

(apply max (keep-indexed (fn [i e] (cond (<= e 9) i :else nil)) [4 7 9 10]))


(reductions + (sort > [1 2 3 4]))

(require '[clojure.math.numeric-tower :as math])

(math/round (/ 300 99))
(length (num-seq 0 299 3N))
(length (range 0 299 3N))
(length (num-seq 0.1 1 (/ 1 11)))

(num-seq 0.005 0.995 0.01)

(concat (num-seq 1 50) (num-seq 50 1))
  

(print (map #(- 51 %) (num-seq 1 51)))

(sum (samples-with-prob [0 1] 100000 [2 8]))

(pow (num-seq 0 10 0.1) 2.0)

(minus (num-seq 0.01 1 0.01) 1)
 
(ccmo/-  1)

(Math/round (/ 12 13))








(ns bayeswork.core
  (:use incanter.core)
  (:use incanter.stats)
  (:use incanter.charts)
  (:require [clojure.math.numeric-tower :as math]))

(defn plot-ptheta 
  "Plots dbeta priors"
  [a b]
  (let [theta (range 0.01 1 0.01)
        p-theta (pdf-beta theta :alpha a :beta b)]
    (xy-plot theta p-theta)))

(defn pdata
  "computes the evidence for the model; p(D|M) or p(z,N)"
  [a b z n]
  (let [theta (range 0.01 1 0.01)]
    (/ (beta (+ z a) (+ (- n z) b))
       (beta a b))))

(defn samples-with-prob 
  "takes size samples with weights from prob, like the R func sample with replace=TRUE" 
  [samples size prob]
  (let [intervals (rest (reductions + 0 (div prob (sum prob))))
        unif-samples (repeatedly size rand)
        the-indices (map (fn [s] (length (take-while #(> s %) intervals))) unif-samples)]
    (map #(nth samples %) the-indices)))

(defn sim-sample-z 
  "computes a single sample z based on a prior beta and actual data z, n"
  [a b z n]
  (let [post-a (+ a z)
        post-b (- (+ b n) z)
        sample-theta (sample-beta 1 :alpha post-a :beta post-b)
        sample-data (samples-with-prob [0 1] n [(- 1.0 sample-theta) sample-theta])]
    (sum sample-data)))

(defn beta-posterior-predictions [a b z n]
  (repeatedly 10000 #(sim-sample-z a b z n)))

(defn plot-p-data-given-theta 
  "plots the likelihood of the data for a range of thetas"
  [n z]
  (let [theta (range 0.01 1 0.01)
        p-data-given-theta (map * (pow theta z) (pow (minus 1 theta) (- n z)))]
    (xy-plot theta p-data-given-theta)))

(defn not-yet-reached [item last incr]
  (cond
   (> incr 0) (<= item last)
   (< incr 0) (>= item last)
   :else false))

(defn num-seq [start last & [incr]]
  (let [incr (cond (> start last) (or incr -1)
                   :else (or incr 1))]
    (take-while #(not-yet-reached % last incr) 
                ((fn nseq [ith]
                   (lazy-seq (cons ith (nseq (+ ith incr)))))
                 start))))

(defn thin-index [nteeth n-to-plot]
  (cond
   (> nteeth n-to-plot) (let [incr (math/round (/ nteeth n-to-plot))
                              thindx (num-seq 0 (- nteeth incr) incr)]
                          (cond
                           (< (length thindx) nteeth) (concat thindx [(dec nteeth)])
                           :else thindx))
   :else (num-seq 0 (dec nteeth))))

(defn bern-grid
  "Chapter 6 function that plots discrete beta stuff"
  [theta p-theta data n-to-plot]
  (let [z (count (filter #(== 1 %) data))
        n (length data)
        p-data-given-theta (map * (pow theta z) (pow (minus 1 theta) (- n z))) ;likelihood
        p-data (sum (mult p-theta p-data-given-theta))
        p-theta-given-data (div (mult p-data-given-theta p-theta) p-data)
        thindx (thin-index (length theta) n-to-plot)
        thinned-theta (map #(nth theta %) thindx)
        thinned-p-theta (map #(nth p-theta %) thindx)
        thinned-p-data-given-theta (map #(nth p-data-given-theta %) thindx)
        thinned-p-theta-given-data (map #(nth p-theta-given-data %) thindx)]
    [thinned-theta thinned-p-theta thinned-p-data-given-theta thinned-p-theta-given-data]))

(defn hdi-of-grid 
  "from section 23.3 of the book.  returns a map of indices, the mass, and the hdi height"
  [p-mass-vec & [cred-mass]]
  (let [cred-mass (or cred-mass 0.95) 
        sorted-mass (sort > p-mass-vec)
        cum-sums (reductions + sorted-mass)
        ;; hdi-height-index (apply max (for [i (range 0 (dec (length cum-sums))) 
        ;;                                   :while (<= (nth cum-sums i) cred-mass)] i))
        hdi-height-index (apply max (keep-indexed (fn [i e] (cond (<= e cred-mass) i :else nil)) cum-sums))
        hdi-height (nth sorted-mass hdi-height-index)
        hdi-mass (sum (filter #(>= % hdi-height) p-mass-vec))
        indices (keep-indexed (fn [ind elem] (cond (>= elem hdi-height) ind :else nil)) p-mass-vec)]
    {:indices indices :mass hdi-mass :height hdi-height}))

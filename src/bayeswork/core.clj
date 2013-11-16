(ns bayeswork.core
  (:refer-clojure :exclude [* / - + == ])
  ;; (:require [clojure.core.matrix :as ccm])
  ;; (:require [clojure.core.matrix.operators :as ccmo])
  (:use incanter.core)
  (:use incanter.stats)
  (:use incanter.charts))

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

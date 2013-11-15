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

(defn plot-p-data-given-theta 
  "plots the likelihood of the data for a range of thetas"
  [n z]
  (let [theta (range 0.01 1 0.01)
        p-data-given-theta (map * (pow theta z) (pow (minus 1 theta) (- n z)))]
    (xy-plot theta p-data-given-theta)))

(ns bayeswork.core
  (:refer-clojure :exclude [* / - + == ])
  ;; (:require [clojure.core.matrix :as ccm])
  ;; (:require [clojure.core.matrix.operators :as ccmo])
  (:use incanter.core)
  (:use incanter.stats)
  (:use incanter.charts))

(defn plotbeta [a b]
  (let [theta (range 0.01 1 0.01)
        beta (pdf-beta theta :alpha a :beta b)]
    (xy-plot theta beta)))

(defn plotprior [n z]
  (let [theta (range 0.01 1 0.01)
        p-data-given-theta (map * (pow theta z) (pow (minus 1 theta) (- n z)))]
    (xy-plot theta p-data-given-theta)))

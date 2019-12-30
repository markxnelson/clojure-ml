(ns ch1.core
  (:use clojure.core.matrix)
  (:require [clatrix.core :as cl])
  (:gen-class))

(defn square-matrix
  "Creates a square matrix of size n x n
  whose elements are all e"
  [n e & {:keys [implementation]
          :or {implementation :persistent-vector}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(defn id-matrix
  "Creates and identity matrix of size n x n"
  [n]
  (let [init (square-matrix n 0 :implementation :clatrix)
        identity-f (fn [i j n]
                     (if (= i j) 1 n))]
    (cl/map-indexed identity-f init)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

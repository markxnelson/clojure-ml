(ns ch1.core
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :as M])
  (:require [clatrix.core :as cl])
  (:gen-class))

(defn square-matrix
  "Creates a square matrix of size n x n
  whose elements are all e"
  [n e & {:keys [implementation]
          :or   {implementation :persistent-vector}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(defn id-matrix
  "Creates and identity matrix of size n x n"
  [n]
  (let [init (square-matrix n 0 :implementation :clatrix)
        identity-f (fn [i j n]
                     (if (= i j) 1 n))]
    (cl/map-indexed identity-f init)))

(defn rand-square-clmatrix
  "Generates a random clatrix matrix of size n x n"
  [n]
  (cl/map rand-int (square-matrix n 100 :implementation :clatrix)))

(defn rand-square-matrix
  "Generates a random matrix of size n x n"
  [n]
  (matrix
    (repeatedly n #(map rand-int (repeat n 100)))))

(defn id-computed-matrix
  "Creates an identity matrix of size n x n
  using compute-matrix"
  [n]
  (compute-matrix [n n] #(if (= %1 %2) 1 0)))

(defn rand-computed-matrix
  "Creates an n x m matrix of random elements
  using compute-matrix"
  [n m]
  (compute-matrix [n m]
                  (fn [i j] (rand-int 100))))

(defn matrix-eq
  "Checks if two matrices are equal"
  [A B]
  (and (= (count A) (count B))
       (reduce #(and %1 %2) (map = A B))))

(defn matrix-add
  "Add two or more matrices"
  ([A B]
   (mapv #(mapv + %1 %2) A B))
  ([A B & more]
   (let [M (concat [A B] more)]
     (reduce matrix-add M))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

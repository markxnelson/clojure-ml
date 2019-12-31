(ns ch1.core
  (:use clojure.core.matrix)
  (:use [incanter.charts :only [xy-plot add-points]]
        [incanter.core :only [view]])
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

(defn matrix-trace
  "Compute the trace of a matrix"
  [A]
  (loop [i 0 result 0]
    (if (>= i (count A))
      result
      (recur (inc i) (+ result (cl/get A i i))))))

(defn time-mat-mul
  "Measures the time for multiplication of two matrices A and B"
  [A B]
  (time (M/* A B)))

(defn core-matrix-mul-time []
  (let [A (rand-square-matrix 100)
        B (rand-square-matrix 100)]
    (time-mat-mul A B)))

(defn clatrix-mul-time []
  (let [A (rand-square-clmatrix 100)
        B (rand-square-clmatrix 100)]
    (time-mat-mul A B)))

; -------

(defn lmatrix [n]
  (compute-matrix :clatrix [n (+ n 2)]
                  (fn [i j] ({0 -1, 1 2, 2 -1} (- j i) 0))))

(defn problem
  "Return a map of the problem setup for a
  given matrix size, number of observed values
  and regularization parameter"
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L (M/* (lmatrix n) lambda)
     :observed (take n-observed i)
     :hidden (drop n-observed i)
     :observed-values (matrix :clatrix
                              (repeatedly n-observed rand))}))

(defn solve
  "Return a map containg the approximated value
  y of each hidden point x"
  [{:keys [L observed hidden observed-values] :as problem}]
  (let [nc (column-count L)
        nr (row-count L)
        L1 (cl/get L (range nr) hidden)
        L2 (cl/get L (range nr) observed)
        l11 (M/* (transpose L1) L1)
        l12 (M/* (transpose L1) L2)]
    (assoc problem :hidden-values
                   (M/* -1 (inverse l11) l12 observed-values))))

(defn plot-points
  "Plots sample points of a solution s"
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view
      (add-points
        (xy-plot X Y) (:observed s) (:observed-values s)))))

(defn plot-rand-sample []
  (plot-points (solve (problem 150 10 30))))

; --------

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(ns adventofcode2016.day16
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn str->bits [s]
  (map #(- (int %) (int \0)) s) )

(defn invert [s]
  (map #(- 1 %) s) )

(defnp unfold [n s]
  (let [v (transient (vec s))]
    (while (< (count v) n)
      (let [n-to-rotate (count v)]
        (conj! v 0)
        (reduce #(if (= (count %) n)
                   (reduced %)
                   (conj! % (- 1 (get % (- (dec n-to-rotate) %2)))) )
                v
                (range n-to-rotate) ) ) )
    (persistent! v) ) )

(defn checksum [n s]
  (if (odd? n)
    s
    (->> s
         (partition 2)
         (map #(if (= (reduce + %) 1) 0 1))
         (recur (/ n 2)) ) ) )

(def part-1-disk-size 272)
(def part-2-disk-size 35651584)
(def disk-size part-2-disk-size)

(defn -main []
  (time (->> "10001110011110000"
             str->bits
             (unfold disk-size)
             (checksum disk-size)
             str/join
             println )) )

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

(defn unfold [s n]
  (let [unfolded-s (reduce into [] [s [0] (invert (reverse s))])]
    (if (< (count unfolded-s) n)
      (recur unfolded-s n)
      (take n unfolded-s) ) ) )

(defn checksum [s]
  (if (odd? (count s))
    s
    (->> s
         (partition 2)
         (map #(if (= (reduce + %) 1) 0 1))
         checksum ) ) )

(defn -main []
  (time (-> "10001110011110000"
            str->bits
            #_(unfold 272) ; Part 1
            (unfold 35651584) ; Part 2
            checksum
            str/join
            println )) )

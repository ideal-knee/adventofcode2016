(ns adventofcode2016.day18
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(def puzzle-input "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^..")

(defn parse-row-string [row-string]
  (map #{\.} row-string) )

(defn trap-next? [[left _ right]]
  (<-> [left right]
       (filter identity <-)
       count
       (if (not= <- 1) \.) ) )

(defn next-row [row]
  (map trap-next? (partition 3 1 (reduce into [\.] [row [\.]]))) )

(defn print-row [row]
  (println (str/join (map #(or % \^) row))) )

(defn count-safe-tiles [{:keys [current-row n-rows-remaining n-safe-tiles]}]
  (if (zero? n-rows-remaining)
    n-safe-tiles
    (recur {:current-row (next-row current-row)
            :n-rows-remaining (dec n-rows-remaining)
            :n-safe-tiles (+ n-safe-tiles (count (filter identity current-row))) }) ) )

(defn -main []
  (time (<-> puzzle-input
             parse-row-string
             (count-safe-tiles {:current-row <- :n-rows-remaining 400000 :n-safe-tiles 0})
             println )) )

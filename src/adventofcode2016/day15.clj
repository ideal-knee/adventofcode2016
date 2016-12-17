(ns adventofcode2016.day15
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn append-num [disc num]
  (update disc :num + num) )

(defn aligned? [{:keys [div num]}]
  (zero? (mod num div)) )

(def test-input [{:div 5, :num 4}
                 {:div 2, :num 1} ])

(def part1-input [{:div 17 :num 15}
                  {:div  3 :num  2}
                  {:div 19 :num  4}
                  {:div 13 :num  2}
                  {:div  7 :num  2}
                  {:div  5 :num  0} ])

(def part2-input (conj part1-input {:div 11 :num 0}))

(defn all-aligned? [input delay]
  (<-> input
       (map append-num <- (repeat delay))
       (every? aligned? <-) ) )

(defn -main []
  (time (let [input (<-> part2-input
                         (map append-num <- (map inc (range))))]
          (-> (drop-while #(not (all-aligned? input %)) (range))
              first
              println ) )) )

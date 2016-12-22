(ns adventofcode2016.day20
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn parse-line [line]
  (let [[start end] (<-> line
                         (re-find #"(\d+)-(\d+)" <-)
                         (drop 1 <-)
                         (map #(Long/parseLong %) <-) )]
    {:start start :end end} ) )

(defn part-1 [current-end {:keys [start end]}]
  (let [first-uncovered (inc current-end)]
    (if (> start first-uncovered)
      (reduced first-uncovered)
      (max current-end end) ) ) )

(defn part-2 [{:keys [current-end sum]} {:keys [start end]}]
  (let [first-uncovered (inc current-end)]
    {:current-end (max current-end end)
     :sum (+ sum (if (> start first-uncovered) (- start first-uncovered) 0)) } ) )

(defn -main []
  (time (<-> "day20-input.txt"
             slurp
             str/split-lines
             (map parse-line <-)
             (sort-by :start <-)
             #_(reduce part-1 0 <-)
             (reduce part-2 {:current-end 0 :sum 0} <-)
             println )) )

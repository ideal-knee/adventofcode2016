(ns adventofcode2016.day8
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(def n-rows 6)
(def n-cols 50)

(def off ".")
(def on "#")

(def initial-screen (->> off
                         (repeat n-cols)
                         vec
                         (repeat n-rows)
                         vec ))

(defn print-screen [screen]
  (->> screen
       (map #(apply str %))
       (str/join "\n")
       println ) )

(defn activate-row [row-index n screen]
  (if (= n 0)
    screen
    (-> (activate-row row-index (dec n) screen)
        (assoc-in [row-index (dec n)] on) ) ) )

(defn activate-rect [n-cols n-rows screen]
  (if (= n-rows 0)
    screen
    (->> (activate-rect n-cols (dec n-rows) screen)
         (activate-row (dec n-rows) n-cols) ) ) )

(defn transpose [m]
  (apply mapv vector m) )

(defn rotate [i screen]
  (let [n (count screen)]
    (->> screen
         cycle
         (drop (- n i))
         (take n)
         vec ) ) )

(defn rotate-row [row-index n screen]
  (update screen row-index #(rotate n %)) )

(defn rotate-col [col-index n screen]
  (->> screen
       transpose
       (rotate-row col-index n)
       transpose ) )

(defn process-rect-line [screen line]
  (if-let [groups (re-find #"rect (\d+)x(\d+)" line)]
    (let [[n-cols n-rows] (map #(Long/parseLong %) (drop 1 groups))]
      (activate-rect n-cols n-rows screen) ) ) )

(defn process-rotate-row-line [screen line]
  (if-let [groups (re-find #"rotate row y=(\d+) by (\d+)" line)]
    (let [[row-index n] (map #(Long/parseLong %) (drop 1 groups))]
      (rotate-row row-index n screen) ) ) )

(defn process-rotate-col-line [screen line]
  (if-let [groups (re-find #"rotate column x=(\d+) by (\d+)" line)]
    (let [[col-index n] (map #(Long/parseLong %) (drop 1 groups))]
      (rotate-col col-index n screen) ) ) )

(defn process-line [screen line]
  (some #(% screen line) [process-rect-line
                          process-rotate-row-line
                          process-rotate-col-line ]) )

(defn process-lines [lines]
  (reduce process-line initial-screen lines) )

(defn check-row-voltage [row]
  (count (filter #(= % on) row)) )

(defn check-voltage [screen]
  (->> screen
       (map check-row-voltage)
       (reduce +) ) )

 (defn -main []
  (->> "day8-input.txt"
       slurp
       str/split-lines
       process-lines
       #_check-voltage ; Part 1
       #_println ; Part 1
       print-screen ; Part 2
       ) )

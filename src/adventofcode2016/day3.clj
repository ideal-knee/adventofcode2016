(ns adventofcode2016.day3
  (:require [clojure.string :as s]) )

(defn -main []
  (->> "day3-input.txt"
       slurp
       s/split-lines
       (map #(->> %
                  (re-seq #"\d+")
                  (map (fn [s] (Long/parseLong s))) ))
       (partition 3) ; Part 2
       (mapcat #(apply map vector %)) ; Part 2
       (filter #(let [sorted-lengths (sort %)]
                  (> (apply + (take 2 sorted-lengths)) (nth sorted-lengths 2)) ))
       count
       println ) )

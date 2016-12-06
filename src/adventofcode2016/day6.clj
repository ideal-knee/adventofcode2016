(ns adventofcode2016.day6
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(defn -main []
  (->> "day6-input.txt"
       slurp
       str/split-lines
       (apply map #(seq %&))
       #_(map #(apply max-key second (frequencies %))) ; Part 1
       (map #(apply min-key second (frequencies %))) ; Part 2
       (map first)
       (apply str)
       println ) )

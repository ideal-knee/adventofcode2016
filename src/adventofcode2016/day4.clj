(ns adventofcode2016.day4
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(defn calculate-checksum [encrypted-name]
  (->> encrypted-name
       (re-seq #"\w")
       (group-by identity)
       sort
       (sort-by #(- (count (second %))))
       (map first)
       (take 5)
       (apply str) ) )

(defn shift-char [c]
  (cond (= c \z) \a
        (= c \-) \-
        :else (-> c
                  int
                  inc
                  char ) ) )

(defn shift-string [n s]
  (if (= n 0)
    s
    (->> s
         (map shift-char)
         (apply str)
         (shift-string (dec n)) ) ) )

(defn process-line [line]
  (let [[_ encrypted-name sector-id-string checksum] (re-find #"(.+)-(\d+)\[(.+)]" line)
        sector-id (Long/parseLong sector-id-string) ]
    (if (= (calculate-checksum encrypted-name) checksum)
      {:name (shift-string sector-id encrypted-name)
       :sector-id sector-id } ) ) )

(defn -main []
  (->> "day4-input.txt"
       slurp
       str/split-lines
       (keep process-line)
       #_(map :sector-id) ; Part 1
       #_(apply +) ; Part 1
       (filter #(re-find #"north" (:name %))) ; Part 2
       println ) )

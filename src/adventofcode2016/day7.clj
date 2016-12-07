(ns adventofcode2016.day7
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(defn outside-brackets-seq [s]
  (map #(nth % 2) (re-seq #"(^|])(.*?)(\[|$)" s)) )

(defn inside-brackets-seq [s]
  (map second (re-seq #"\[(.*?)]" s)) )

(defn abba? [[a b c d]]
  (and (= a d) (= b c) (not= a b)) )

(defn has-abba? [s]
  (some abba? (partition 4 1 s)) )

(defn supports-tls? [s]
  (and (some has-abba? (outside-brackets-seq s))
       (not (some has-abba? (inside-brackets-seq s)) ) ) )

(defn aba? [[a b c]]
  (and (= a c) (not= a b)) )

(defn aba-bab? [[[a b _] [c d _]]]
  (and (= a d) (= b c)) )

(defn supports-ssl? [s]
  (some aba-bab? (for [i (filter aba? (mapcat #(partition 3 1 %) (inside-brackets-seq s)))
                       o (filter aba? (mapcat #(partition 3 1 %) (outside-brackets-seq s))) ]
                   [i o] )) )

(defn -main []
  (->> "day7-input.txt"
       slurp
       str/split-lines
       #_(filter supports-tls?) ; Part 1
       (filter supports-ssl?) ; Part 2
       count
       println ) )

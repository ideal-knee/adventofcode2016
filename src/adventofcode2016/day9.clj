(ns adventofcode2016.day9
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(defn parse [s]
  (if-let [match (re-find #"(.*?)\((\d+)x(\d+)\)(.*)" s)]
    (<-> match
         (drop 1 <-)
         (map #(%2 %) <- [identity
                          #(Long/parseLong %)
                          #(Long/parseLong %)
                          identity ]) ) ) )

(defn decompress [s]
  (if-let [[prefix len n rest] (parse s)]
    (let [to-repeat (subs rest 0 len)
          suffix (subs rest len) ]
      (str prefix (apply str (repeat n to-repeat)) (decompress suffix)) )
    s ) )

(def decompress-v2-count
  (memoize (fn [s]
             (if-let [[prefix len n rest] (parse s)]
               (let [to-repeat (subs rest 0 len)
                     suffix (subs rest len) ]
                 (+ (count prefix)
                    (decompress-v2-count (apply str (repeat n to-repeat)))
                    (decompress-v2-count suffix) ) )
               (count s) ) )) )

 (defn -main []
  (<-> "day9-input.txt"
       slurp
       #_decompress ; Part 1
       #_count ; Part 1
       decompress-v2-count ; Part 2
       println ) )

(ns adventofcode2016.day9
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(defn parse [s]
  (if-let [match (re-find #"(.*?)\((\d+)x(\d+)\)(.*)" s)]
    (->> match
         (drop 1)
         (map #(% %2) [identity
                       #(Long/parseLong %)
                       #(Long/parseLong %)
                       identity ]) ) ) )

(defn decompress [s]
  (if-let [[prefix len n rest] (parse s)]
    (let [to-repeat (subs rest 0 len)
          suffix (subs rest len) ]
      (str prefix (apply str (repeat n to-repeat)) (decompress suffix)) )
    s ) )

(declare decompress-v2-count)

(defn decompress-with-suffix [body suffix]
  (let [result (decompress-v2-count body)]
    (if (< result 0)
      (let [n-needed (Math/abs result)]
        (if (< (count suffix) n-needed)
          [(- (count suffix) n-needed)]
          (recur (str body (subs suffix 0 n-needed)) (subs suffix n-needed)) ))
      [result suffix] ) ) )

(def decompress-v2-count
  (memoize (fn [s]
             (if-let [[prefix len n rest] (parse s)]
               (if (< (count rest) len)
                 (- (count rest) len)
                 (let [body (apply str (repeat n (subs rest 0 len)))
                       initial-suffix (subs rest len)
                       [body-decompress-count suffix] (decompress-with-suffix body initial-suffix) ]
                   (if (< body-decompress-count 0)
                     body-decompress-count
                     (+ (count prefix)
                        body-decompress-count
                        (decompress-v2-count suffix) ) ) ) )
               (count s) ) )) )

 (defn -main []
   (-> "day9-input.txt"
       slurp
       #_decompress ; Part 1
       #_count ; Part 1
       decompress-v2-count ; Part 2
       println ) )

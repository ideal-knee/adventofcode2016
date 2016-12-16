(ns adventofcode2016.day14
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn valid-key? [[index [first-hash & rest-hashes]]]
  (if-let [match (re-find #"(.)\1{2}" first-hash)]
    (let [tripled-char (second match)
          quint-char-regex (re-pattern (apply str (repeat 5 tripled-char))) ]
      (some #(re-find quint-char-regex %) rest-hashes) ) ) )

(defn stretch
  ([s] (stretch s 2017))
  ([s n]
   (if (= n 0)
     s
     (recur (d/md5 s) (dec n)) ) ) )

(defn generate [salt]
  (->> (range)
       (map #(str salt %))
       #_(map d/md5) ; Part 1
       (map stretch) ; Part 2
       (partition 1001 1)
       (map vector (range))
       (filter valid-key?)
       (drop 63)
       first
       first
       println ) )

(defn -main []
  (time (generate "qzyelonm")) )

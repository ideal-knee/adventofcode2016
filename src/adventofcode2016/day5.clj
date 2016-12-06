(ns adventofcode2016.day5
  (:require [clojure.string :as str]
            [digest :as d]
            [adventofcode2016.utils :refer [<->]] ) )

(defn hash-seq [prefix]
  (map #(d/md5 (str prefix %)) (range)) )

(defn part-1 [<-]
  (<-> <-
       (take 8 <-)
       (map #(let [[_ c] (re-find #"^00000(.)" %)] c) <-)
       (apply str <-)
       println ) )

(def valid-keys (apply sorted-set (map str (range 8))))

(defn print-password [p]
  (println (apply str (map #(if (p %) (p %) "_") valid-keys))) )

(defn handle-hash [password hash]
  (let [[_ k v] (re-find #"^00000(.)(.)" hash)]
    (if (and (valid-keys k) (not (password k)))
      (let [new-password (assoc password k v)]
        (print-password new-password)
        (if (= (count new-password) (count valid-keys))
          (reduced new-password)
          new-password ) )
      password ) ) )

(defn part-2 [<-]
  (reduce handle-hash {} <-) )

(defn -main []
  (<-> "cxdnnyjw"
       hash-seq
       (filter #(re-find #"^00000" %) <-)
       #_part-1
       part-2 ) )

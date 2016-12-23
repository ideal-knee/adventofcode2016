(ns adventofcode2016.day21
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn string-swap [s index-a index-b]
  (let [char-a (get s index-a)
        char-b (get s index-b) ]
    (<-> s
         vec
         (assoc <- index-a char-b)
         (assoc <- index-b char-a)
         (apply str <-) ) ) )

(defn process-swap-position-line [s line]
  (if-let [match (re-find #"swap position (\d+) with position (\d+)" line)]
    (let [[index-a index-b] (map #(Long/parseLong %) (rest match))]
      (string-swap s index-a index-b) ) ) )

(defn process-swap-letter-line [s line]
  (if-let [match (re-find #"swap letter (.) with letter (.)" line)]
    (let [[char-a char-b] (rest match)
          index-a (str/index-of s char-a)
          index-b (str/index-of s char-b) ]
      (string-swap s index-a index-b) ) ) )

(defn process-reverse-position-line [s line]
  (if-let [match (re-find #"reverse positions (\d+) through (\d+)" line)]
    (let [[start-index end-index] (map #(Long/parseLong %) (rest match))]
      (str (subs s 0 start-index)
           (apply str (reverse (subs s start-index (inc end-index))))
           (subs s (inc end-index)) ) ) ) )

(defn rotate-left [s n]
  (if (= n 0)
    s
    (recur (str (subs s 1) (get s 0)) (dec n)) ) )

(defn rotate-right [s n]
  (if (= n 0)
    s
    (let [index-of-last-char (dec (count s))]
      (recur (str (get s index-of-last-char) (subs s 0 index-of-last-char)) (dec n)) ) ) )

(defn process-rotate-steps-line [s line]
  (if-let [match (re-find #"rotate (\w+) (\d+) steps?" line)]
    (let [[direction amount] (map #(% %2) [keyword #(Long/parseLong %)] (rest match))]
      (if (= direction :left)
        (rotate-left s amount)
        (rotate-right s amount) ) ) ) )

(defn move [s from-index to-index]
  (let [from-char (get s from-index)
        s-without-from-char (str (subs s 0 from-index) (subs s (inc from-index))) ]
    (str (subs s-without-from-char 0 to-index) from-char (subs s-without-from-char to-index))) )

(defn process-move-position-line [s line]
  (if-let [match (re-find #"move position (\d+) to position (\d+)" line)]
    (let [[from-index to-index] (map #(Long/parseLong %) (rest match))]
      (move s from-index to-index) ) ) )

(defn process-rotate-by-position-line [s line]
  (if-let [match (re-find #"rotate based on position of letter (\w)" line)]
    (let [letter-to-rotate-by (second match)
          index-to-rotate-by (str/index-of s letter-to-rotate-by) ]
      (rotate-right s (+ 1
                         index-to-rotate-by
                         (if (>= index-to-rotate-by 4) 1 0) )) ) ) )

(defn process-line [s line]
  (some #(% s line) [process-move-position-line
                     process-reverse-position-line
                     process-rotate-by-position-line
                     process-rotate-steps-line
                     process-swap-letter-line
                     process-swap-position-line
                     #(do (println "FAILED TO PROCESS:" %2) %) ]) )

(defn reverse-process-rotate-by-position-line [s line]
  (if-let [match (re-find #"rotate based on position of letter (\w)" line)]
    (let [letter-to-rotate-by (second match)]
      (<-> (keep (fn [index-to-rotate-by]
                   (let [unrotated-s (rotate-left s (+ 1 index-to-rotate-by (if (>= index-to-rotate-by 4) 1 0)))]
                     (if (= (str/index-of unrotated-s letter-to-rotate-by) index-to-rotate-by)
                       unrotated-s ) ) )
                 (range (count s)) )
           (if (> (count <-) 1)
             (do
               (println "ERROR multiple valid resuts!")
               [] )
             (first <-) ) ) ) ) )

(defn reverse-process-move-position-line [s line]
  (if-let [match (re-find #"move position (\d+) to position (\d+)" line)]
    (let [[from-index to-index] (map #(Long/parseLong %) (rest match))]
      (move s to-index from-index) ) ) )

(defn reverse-process-rotate-steps-line [s line]
  (if-let [match (re-find #"rotate (\w+) (\d+) steps?" line)]
    (let [[direction amount] (map #(% %2) [keyword #(Long/parseLong %)] (rest match))]
      (if (= direction :right)
        (rotate-left s amount)
        (rotate-right s amount) ) ) ) )

(defn reverse-process-line [s line]
  (some #(% s line) [reverse-process-move-position-line
                     process-reverse-position-line
                     reverse-process-rotate-by-position-line
                     reverse-process-rotate-steps-line
                     process-swap-letter-line
                     process-swap-position-line
                     #(do (println "FAILED TO PROCESS:" %2) %) ]) )

(defn part-1 []
  (time (<-> "day21-input.txt"
             slurp
             str/split-lines
             (reduce process-line "abcdefgh" <-)
             println )) )

(defn part-2 []
  (time (<-> "day21-input.txt"
             slurp
             str/split-lines
             reverse
             (reduce reverse-process-line "fbgdceah" <-)
             println )) )

(defn -main []
  (part-2) )

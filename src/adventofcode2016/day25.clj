(ns adventofcode2016.day25
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]] )
  (:gen-class) )

(defn resolve-value [value {:keys [registers]}]
  (if (re-find #"\d+" value)
    (Long/parseLong value)
    (registers value) ) )

(defn process-cpy-command [line state]
  (if-let [match (re-find #"cpy (\S+) (\S+)" line)]
    (let [[value receiver] (map #(% %2) [#(resolve-value % state) identity] (drop 1 match))]
      (-> state
           (update :registers assoc receiver value)
           (update :program-counter inc) ) ) ) )

(defn process-inc-command [line state]
  (if-let [match (re-find #"inc (\S+)" line)]
    (let [receiver (second match)]
      (-> state
          (update-in [:registers receiver] inc)
          (update :program-counter inc) ) ) ) )

(defn process-dec-command [line state]
  (if-let [match (re-find #"dec (\S+)" line)]
    (let [receiver (second match)]
      (-> state
          (update-in [:registers receiver] dec)
          (update :program-counter inc) ) ) ) )

(defn process-jnz-command [line state]
  (if-let [match (re-find #"jnz (\S+) (\S+)" line)]
    (let [[cond amount] (map #(% %2) [#(resolve-value % state) #(resolve-value % state)] (drop 1 match))]
      (if (zero? cond)
        (update state :program-counter inc)
        (-> state
            (update :program-counter + amount) )) ) ) )

(defn process-tgl-command [line {:keys [program program-counter] :as state}]
  (if-let [match (re-find #"tgl (\S+)" line)]
    (let [offset (resolve-value (second match) state)
          index-to-tgl (+ program-counter offset) ]
      (if (< index-to-tgl (count program))
        (let [line-to-tgl (program index-to-tgl)
              cmd-to-tgl (subs line-to-tgl 0 3)
              rest-line-to-tgl (subs line-to-tgl 3)
              tgled-cmd ({"inc" "dec"
                          "dec" "inc"
                          "tgl" "inc"
                          "jnz" "cpy"
                          "cpy" "jnz" } cmd-to-tgl) ]
          (-> state
              (update :program-counter inc)
              (update  :program assoc #_assoc! index-to-tgl (str tgled-cmd rest-line-to-tgl)) ) )
        (update state :program-counter inc) ) ) ) )

(defn process-out-command [line state]
  (if-let [match (re-find #"out (\S+)" line)]
    (let [signal (resolve-value (second match) state)]
      (-> state
          (update :program-counter inc)
          (update :signals conj signal) ) ) ) )

(defn process-line [line state]
  (some #(% line state) [process-cpy-command
                         process-inc-command
                         process-dec-command
                         process-jnz-command
                         process-tgl-command
                         process-out-command ]) )

(def last-printed (atom 0))

(defn process-current-line [{:keys [program program-counter] :as state}]
  #_(let [current-seconds (.getEpochSecond (java.time.Instant/now))]
      (if (not= current-seconds @last-printed)
        (do
          (println (:registers state) (:signals state))
          (println (<-> program
                        (map (fn [line-number line] (str (format "%2d" line-number) (if (= line-number program-counter) "->" "  ") line)) (range) <-)
                        (str/join "\n" <-) ))
          (reset! last-printed current-seconds) ) ) )
  (process-line (program program-counter) state) )

(defn run-program-aux [{:keys [program program-counter registers signals] :as state} n-signals]
  (if (and (< program-counter (count program)) (< (count signals) n-signals))
    (recur (process-current-line state) n-signals)
    signals ) )

(defn initial-registers [initial-a] {"a" initial-a "b" 0 "c" 0 "d" 0})

(defn run-program [program initial-a n-signals]
  (<-> program
       #_transient
       (run-program-aux {:program <- :program-counter 0 :registers (initial-registers initial-a) :signals []} n-signals) ) )

(defn -main [n-signals & args]
  (time (let [program (-> "day25-input.txt"
                          slurp
                          str/split-lines )
              n-signals (Long/parseLong n-signals) ]
          (println (reduce (fn [_ initial-a]
                             (let [current-seconds (.getEpochSecond (java.time.Instant/now))]
                               (if (not= current-seconds @last-printed)
                                 (do
                                   (println initial-a)
                                   (reset! last-printed current-seconds) ) ) )
                             (let [result (run-program program initial-a n-signals)]
                               (if (= (persistent! (reduce conj! (transient #{}) (partition 2 result))) #{[0 1]})
                                 (reduced initial-a) ) )  )
                           nil
                           (range) )) )) )

(ns adventofcode2016.day12
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
  (if-let [match (re-find #"cpy (\w+) (\w+)" line)]
    (let [[value receiver] (map #(% %2) [#(resolve-value % state) identity] (drop 1 match))]
      (-> state
           (update :registers assoc receiver value)
           (update :program-counter inc) ) ) ) )

(defn process-inc-command [line state]
  (if-let [match (re-find #"inc (\w+)" line)]
    (let [receiver (second match)]
      (-> state
          (update-in [:registers receiver] inc)
          (update :program-counter inc) ) ) ) )

(defn process-dec-command [line state]
  (if-let [match (re-find #"dec (\w+)" line)]
    (let [receiver (second match)]
      (-> state
          (update-in [:registers receiver] dec)
          (update :program-counter inc) ) ) ) )

(defn process-jnz-command [line state]
  (if-let [match (re-find #"jnz (\w+) ([-\d]+)" line)]
    (let [[cond amount] (map #(% %2) [#(resolve-value % state) #(resolve-value % state)] (drop 1 match))]
      (if (zero? cond)
        (update state :program-counter inc)
        (-> state
            (update :program-counter + amount) )) ) ) )

(defn process-line [line state]
  (some #(% line state) [process-cpy-command
                         process-inc-command
                         process-dec-command
                         process-jnz-command ]) )

(defn process-current-line [{:keys [program program-counter] :as state}]
  (process-line (program program-counter) state) )

(defn run-program-aux [{:keys [program program-counter registers] :as state}]
  (if (< program-counter (count program))
    (recur (process-current-line state))
    registers ) )

(def initial-registers {"a" 0 "b" 0 "c" 0 "d" 0})

(defn run-program [program]
  (run-program-aux {:program program :program-counter 0 :registers initial-registers}) )

(defn -main []
  (-> "day12-input.txt"
      slurp
      str/split-lines
      run-program
      println ) )

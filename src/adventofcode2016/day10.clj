(ns adventofcode2016.day10
  (:require [clojure.string :as str]
            [adventofcode2016.utils :refer [<->]] ) )

(def initial-state {:nodes {} :edges {}})

(defn bot-name->key [s]
  (map keyword (str/split s #"\s+")) )

(defn process-gives [{:keys [nodes edges]} line]
  (if-let [match (re-find #"(.*) gives low to (.*) and high to (.*)" line)]
    (let [[sender lo-receiver hi-receiver] (map bot-name->key (drop 1 match))]
      {:nodes nodes
       :edges (-> edges
                  (assoc-in [sender :lo] lo-receiver)
                  (assoc-in [sender :hi] hi-receiver) )}) ) )

(defn process-goes-to [{:keys [nodes edges]} line]
  (if-let [match (re-find #"value (.*) goes to (.*)" line)]
    (let [[value bot] (map #(% %2) [#(Long/parseLong %)  bot-name->key] (drop 1 match))]
      {:nodes (-> nodes
                  (update-in bot #(conj (or % #{}) value)) )
       :edges edges }) ) )

(defn process-line [state line]
  (some #(% state line) [process-gives
                         process-goes-to ]) )

(defn process-input [lines]
  (reduce process-line initial-state lines) )

(defn n-outputs [{:keys [edges]}]
  (->> edges
       vals
       (mapcat vals)
       (filter #(= (first %) :output))
       count ) )

(defn give-chips [{:keys [nodes edges]} bot-id]
  (let [[lo-value hi-value] (sort (get-in nodes [:bot bot-id]))
        {lo-bot :lo hi-bot :hi} (edges [:bot bot-id]) ]
    {:nodes (-> nodes
                (update-in lo-bot #(conj (or % #{}) lo-value))
                (update-in hi-bot #(conj (or % #{}) hi-value)) )
     :edges edges } ) )

(defn tick [{:keys [nodes edges] :as state}]
  (let [bot-ids-with-2-values (->> nodes
                                   :bot
                                   (filter #(= (count (second %)) 2))
                                   (map first) )]
    (reduce give-chips state bot-ids-with-2-values) ) )

(defn run [state]
  (if (= (count (get-in state [:nodes :output])) (n-outputs state))
    state
    (run (tick state)) ) )

(defn part1 [state]
  (->> state
       :nodes
       :bot
       (filter #(= (second %) #{61 17}))
       first
       first
       name ) )

(defn part2 [state]
  (->> state
       :nodes
       :output
       (sort-by #(Long/parseLong (name (first %))))
       (take 3)
       (map #(first (second %)))
       (reduce *) ) )

(defn -main []
  (->> "day10-input.txt"
       slurp
       str/split-lines
       process-input
       run
       part2
       println ) )

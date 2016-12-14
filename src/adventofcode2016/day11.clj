(ns adventofcode2016.day11
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]] )
  (:gen-class) )

(tufte/add-basic-println-handler! {})

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(def test-initial-state {:elevator-floor 0
                         :floors [{:generators #{}
                                   :microchips #{:hydrogen :lithium} }
                                  {:generators #{:hydrogen}
                                   :microchips #{} }
                                  {:generators #{:lithium}
                                   :microchips #{} }
                                  {:generators #{}
                                   :microchips #{} } ]})

(def part1-initial-state {:elevator-floor 0
                          :floors [{:generators #{:strontium :plutonium}
                                    :microchips #{:strontium :plutonium} }
                                   {:generators #{:thulium :ruthenium :curium}
                                    :microchips #{:ruthenium :curium} }
                                   {:generators #{}
                                    :microchips #{:thulium} }
                                   {:generators #{}
                                    :microchips #{} } ]})

(def part2-initial-state {:elevator-floor 0
                          :floors [{:generators #{:strontium :plutonium :elerium :dilithium}
                                    :microchips #{:strontium :plutonium :elerium :dilithium} }
                                   {:generators #{:thulium :ruthenium :curium}
                                    :microchips #{:ruthenium :curium} }
                                   {:generators #{}
                                    :microchips #{:thulium} }
                                   {:generators #{}
                                    :microchips #{} } ]})

(def initial-state part2-initial-state)

(def initial-game-tree {:nodes (conj empty-queue {:state initial-state
                                                  :previous-states [] })
                        :previously-seen-states #{initial-state} })

(def n-items (+ (count (mapcat :generators (:floors initial-state)))
                (count (mapcat :microchips (:floors initial-state))) ))

(defnp done? [{[_ _ _ fourth-floor] :floors}]
  (= (+ (count (:generators fourth-floor)) (count (:microchips fourth-floor))) n-items) )

(defnp update-state [state floor-index taking-with]
  (let [previous-floor-index (:elevator-floor state)]
    (<-> state
         transient
         (p ::assoc-elevator-floor (assoc! <- :elevator-floor floor-index))
         (assoc! <- :floors (<-> (get <- :floors)
                                 transient
                                 (p ::set-previous-floor (assoc! <- previous-floor-index (<-> (get <- previous-floor-index)
                                                                                              transient
                                                                                              (assoc! <- :generators (apply disj (get <- :generators) (:generators taking-with)))
                                                                                              (assoc! <- :microchips (apply disj (get <- :microchips) (:microchips taking-with)))
                                                                                              persistent! )))
                                 (p ::set-current-floor (assoc! <- floor-index (<-> (get <- floor-index)
                                                                                    transient
                                                                                    (assoc! <- :generators (apply conj (get <- :generators) (:generators taking-with)))
                                                                                    (assoc! <- :microchips (apply conj (get <- :microchips) (:microchips taking-with)))
                                                                                    persistent! )))
                                 persistent! ))
         persistent! ) ) )

(defnp things-on-current-floor [state]
  (let [current-floor (:elevator-floor state)]
    (into (map #(vector :generators %) (get-in state [:floors current-floor :generators]))
          (map #(vector :microchips %) (get-in state [:floors current-floor :microchips])) ) ) )

(defnp maplist [f s]
  (map f (take-while identity (iterate next s))) ) ; Stolen from https://groups.google.com/d/msg/clojure/9bnpeURy-I8/JzUH2tyf0ecJ

(defnp n-choose-2 [v]
  (apply concat (maplist (fn [[first & rest]] (map #(vector first %) rest)) v)) )

(defnp tuples->map [tuples]
  (reduce (fn [m [k v]] (update m k #(conj (or % #{}) v))) {} tuples) )

(defnp combinations-to-take [state]
  (let [current-floor (:elevator-floor state)
        things (things-on-current-floor state)
        combos (into (map vector things) (n-choose-2 things)) ]
    (map tuples->map combos) ) )

(defnp next-floors [{current-floor :elevator-floor}]
  ({0 [1]
    1 [0 2]
    2 [1 3]
    3 [2] } current-floor ) )

(defnp next-states [state]
  (for [next-floor (next-floors state)
        taking-with (combinations-to-take state) ]
    (update-state state next-floor taking-with) ) )

(defnp valid-floor? [floor]
  (or (zero? (count (:generators floor)))
      (every? (:generators floor) (:microchips floor)) ) )

(defnp valid-state? [state]
  (every? valid-floor? (:floors state)) )

(defn get-seconds []
  (.getEpochSecond (java.time.Instant/now)) )

(def last-print-time (atom (get-seconds)))

(defn bfs [{:keys [nodes previously-seen-states]}]
  (let [{:keys [state previous-states]} (peek nodes)
        rest-game-tree (pop nodes)
        next-states (next-states state) ]
    (let [time (get-seconds)]
      (when (and (= (mod time 10) 0)
                 (not= time @last-print-time) )
        (println "previous-states:" (count previous-states) "rest-game-tree:" (count rest-game-tree))
        (reset! last-print-time time) ) )

    (if-let [final-state (p ::some-done? (some done? next-states))]
      (inc (count previous-states))
      (let [eligible-next-states (p ::eligible-next-states (<-> next-states
                                                                (filter valid-state? <-)
                                                                (remove previously-seen-states <-)))]
        (recur {:nodes (p ::recur-nodes (into rest-game-tree (p ::recur-nodes-map (map #(hash-map :state % :previous-states (conj previous-states state)) eligible-next-states))))
                :previously-seen-states (p ::recur-previously-seen-states (into previously-seen-states eligible-next-states)) }) ) ) ) )

#_
(defn pro []
  (profile {}
           (<-> initial-game-tree
                bfs
                println )) )

(defn -main []
  (<-> initial-game-tree
       bfs
       println ) )

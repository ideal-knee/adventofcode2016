(ns adventofcode2016.day13
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(def start-position {:x 1 :y 1})

(def end-position {:x 31 :y 39})
(def magic-number 1350)

(def display-max-x 45)
(def display-max-y 50)

(def initial-node {:position start-position
                   :depth 0 })

(defn neighboring-positions [{:keys [x y]}]
  [{:x (dec x) :y y      }
   {:x (inc x) :y y      }
   {:x x       :y (dec y)}
   {:x x       :y (inc y)} ] )

(defn in-bounds? [{:keys [x y]}]
  (and (>= x 0) (>= y 0)) )

(defn open-space? [magic-number {:keys [x y]}]
  (->> (+ (* x x) (* x 3) (* x y 2) y (* y y))
       (+ magic-number)
       (Long/toBinaryString)
       (filter #(= % \1))
       count
       even? ) )

(defn cartesian-distance [{a-x :x a-y :y} {b-x :x b-y :y}]
  (Math/pow (+ (Math/pow (Math/abs (- a-x b-x)) 2) (Math/pow (Math/abs (- a-y b-y)) 2)) 0.5) )

(defn cost [end-position {:keys [position depth]}]
  (+ depth (cartesian-distance position end-position)) )

(defn print-state [{:keys [visited-positions magic-number end-position] {:keys [values]} :priority-queue}]
  (let [queued-nodes (into #{} (map #(-> % second :position) values))]
    (->> (range display-max-y)
         (map (fn [y]
                (->> (range display-max-x)
                     (map (fn [x]
                            (let [p {:x x :y y}]
                              (cond (= p end-position) "XX"
                                    (queued-nodes p) "<>"
                                    (visited-positions p) ".."
                                    (not (open-space? magic-number p)) "##"
                                    :else "  " )) ))
                     str/join ) ))
         (str/join "\n")
         println
         (println) ) ) )

(defn a-star [{:keys [priority-queue visited-positions magic-number end-position] :as state}]
  (print-state state)
  (Thread/sleep (long (/ 1000 10)))

  (let [[current-node new-priority-queue] (pq/pq-pop priority-queue)
        {current-position :position current-depth :depth} current-node ]
    (if (= current-position end-position)
      current-depth
      (let [new-positions (->> current-position
                               neighboring-positions
                               (filter in-bounds?)
                               (filter #(open-space? magic-number %))
                               (remove visited-positions) )
            new-nodes (map #(hash-map :position % :depth (inc current-depth)) new-positions) ]
        (recur (assoc state
                      :priority-queue (reduce pq/pq-push new-priority-queue new-nodes)
                      :visited-positions (into visited-positions new-positions) )) ) ) ) )

(defn visit-nodes [current-position current-depth visited-nodes]
  (if (= current-depth 50)
    visited-nodes
    (let [valid-neighbors (->> current-position
                               neighboring-positions
                               (filter in-bounds?)
                               (filter #(open-space? magic-number %))
                               (remove visited-nodes) )
          next-visited-nodes (apply conj visited-nodes valid-neighbors)]
      (reduce #(apply conj % %2) visited-nodes (map #(visit-nodes % (inc current-depth) next-visited-nodes) valid-neighbors)))) )

(defn part-1 []
  (-> (a-star {:priority-queue (pq/pq-push (pq/empty-pq #(cost end-position %)) initial-node)
               :visited-positions #{start-position}
               :magic-number magic-number
               :end-position end-position })
      println ) )

(defn part-2 []
  (-> (visit-nodes start-position 0 #{start-position})
      count
      println ) )

(defn -main []
  (time (part-1))
  (time (part-2)) )

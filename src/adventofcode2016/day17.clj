(ns adventofcode2016.day17
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(def x-size 4)
(def y-size 4)
(def test-1-passcode "ihgpwlah")
(def test-2-passcode "ulqzkmiv")
(def puzzle-input-passcode "njfxhljp")
(def passcode puzzle-input-passcode)

(def initial-position {:x 0 :y 0})
(def initial-node {:position initial-position :path ""})
(def initial-queue (conj clojure.lang.PersistentQueue/EMPTY initial-node))

(defn open-doors [h]
  (keep (fn [[c d]] (if (> (int c) (int \a)) d)) (map vector h [\U \D \L \R])) )

(def door->vector {\U {:x  0 :y -1}
                   \D {:x  0 :y  1}
                   \L {:x -1 :y  0}
                   \R {:x  1 :y  0} })

(defn move [{p-x :x p-y :y} {v-x :x v-y :y}]
  {:x (+ p-x v-x) :y (+ p-y v-y)} )

(defn in-bounds? [{:keys [x y] :as position}]
  (if (and (<  x x-size)
           (>= x      0)
           (<  y y-size)
           (>= y      0) )
    position ) )

(defn build-next-nodes [{current-position :position :as current-node} open-doors]
  (->> open-doors
       (keep #(if-let [new-position (->> %
                                         door->vector
                                         (move current-position)
                                         in-bounds? )]
                (-> current-node
                    (assoc :position new-position)
                    (update :path str %) ) )) ) )

(defn final-node? [{:keys [path] {:keys [x y]} :position}]
  (if (and (= x (dec x-size)) (= y (dec y-size)))
    path ) )

(def max-path (atom ""))

(defn bfs-part1 [queue]
  (let [{current-position :position current-path :path :as current-node} (peek queue)
        rest-nodes (pop queue) ]
    (let [next-nodes (->> current-path
                          (str passcode)
                          d/md5
                          open-doors
                          (build-next-nodes current-node) )]
      (if-let [final-path (some final-node? next-nodes)]
        final-path
        (recur (into rest-nodes next-nodes)) ) ) ) )

(defn bfs-part2 [queue]
  (let [{current-position :position current-path :path :as current-node} (peek queue)
        rest-nodes (pop queue) ]
    (if current-node
      (let [next-nodes (->> current-path
                            (str passcode)
                            d/md5
                            open-doors
                            (build-next-nodes current-node) )]
        (if-let [final-path (final-node? current-node)]
          (do
            (if (> (count final-path) (count @max-path))
              (reset! max-path final-path) )
            (recur rest-nodes) )
          (recur (into rest-nodes next-nodes))) )
      (count @max-path) ) ) )

(defn -main []
  (time (-> (bfs-part1 initial-queue)
            println ))

  (time (-> (bfs-part2 initial-queue)
            println )) )

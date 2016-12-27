(ns adventofcode2016.day22
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn process-line [line]
  (if-let [match (re-find #"/dev/grid/node-x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%" line)]
    (let [[x y used avail] (map #(Long/parseLong %) (rest match))]
      {:x x :y y :used used :avail avail} ) ) )

(defn get-nodes []
  (<-> "day22-input.txt"
       slurp
       str/split-lines
       (drop 2 <-)
       (map process-line <-) ) )

#_
(defn get-nodes []
  (<-> ["/dev/grid/node-x0-y0   10T    8T     2T   80%"
        "/dev/grid/node-x0-y1   11T    6T     5T   54%"
        "/dev/grid/node-x0-y2   32T   28T     4T   87%"
        "/dev/grid/node-x1-y0    9T    7T     2T   77%"
        "/dev/grid/node-x1-y1    8T    0T     8T    0%"
        "/dev/grid/node-x1-y2   11T    7T     4T   63%"
        "/dev/grid/node-x2-y0   10T    6T     4T   60%"
        "/dev/grid/node-x2-y1    9T    8T     1T   88%"
        "/dev/grid/node-x2-y2    9T    6T     3T   66%"]
       (map process-line <-) ) )

(defn part-1 []
  (let [nodes (get-nodes)]
    (<-> (for [node-a nodes
               node-b nodes
               :when (and (not= node-a node-b)
                          (> (:used node-a) 0)
                          (<= (:used node-a) (:avail node-b)) ) ]
           nil )
         count ) ) )

(defn part-2-process-line [line]
  (if-let [match (re-find #"/dev/grid/node-x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%" line)]
    (let [[x y used avail] (map #(Long/parseLong %) (rest match))]
      {:x x :y y :used (cond (zero? used) :zero
                             (> used 200) :too-much
                             :else :some )} ) ) )

(defn part-2-get-nodes []
  (<-> "day22-input.txt"
       slurp
       str/split-lines
       (drop 2 <-)
       (map part-2-process-line <-) ) )

#_
(defn part-2-get-nodes []
  (<-> ["/dev/grid/node-x0-y0   10T    8T     2T   80%"
        "/dev/grid/node-x0-y1   11T    6T     5T   54%"
        "/dev/grid/node-x0-y2   32T   28T     4T   87%"
        "/dev/grid/node-x1-y0    9T    7T     2T   77%"
        "/dev/grid/node-x1-y1    8T    0T     8T    0%"
        "/dev/grid/node-x1-y2   11T    7T     4T   63%"
        "/dev/grid/node-x2-y0   10T    6T     4T   60%"
        "/dev/grid/node-x2-y1    9T    8T     1T   88%"
        "/dev/grid/node-x2-y2    9T    6T     3T   66%"]
       (map part-2-process-line <-) ) )

(defn build-node-map [nodes]
  (reduce (fn [m {:keys [x y] :as node}] (assoc-in m [x y] node)) {} nodes) )

(defn all-nodes [node-map]
  (mapcat vals (vals node-map)) )

(defn valid-next-states [{:keys [node-map goal-x goal-y] :as state} {:keys [x y used] :as node}]
  (keep (fn [[neighbor-x neighbor-y]]
          (if-let [neighbor (get-in node-map [neighbor-x neighbor-y])]
            (if (and (#{:some} (:used neighbor)) (#{:zero} used))
              {:node-map (<-> node-map
                              (assoc-in <- [x y :used] :some)
                              (assoc-in <- [neighbor-x neighbor-y :used] :zero) )
               :goal-x (if (and (= goal-x neighbor-x) (= goal-y neighbor-y)) x goal-x)
               :goal-y (if (and (= goal-x neighbor-x) (= goal-y neighbor-y)) y goal-y) } ) ) )
        [[(inc x) y]
         [(dec x) y]
         [x (inc y)]
         [x (dec y)] ] ) )

(defn all-valid-next-states [state]
  (mapcat #(valid-next-states state %) (all-nodes (:node-map state))) )

(def initial-state (<-> (part-2-get-nodes)
                        build-node-map
                        (hash-map :node-map <- :goal-x 36 :goal-y 0) ))

(def initial-a-star-node {:state initial-state :n-steps 0})

(defn euclidean-distance [a-x a-y b-x b-y]
  (Math/pow (+ (Math/pow (- a-x b-x) 2) (Math/pow (- a-y b-y) 2)) 0.5) )

(defn cost-fn [{{:keys [goal-x goal-y node-map]} :state :keys [n-steps]}]
  (let [distance-to-swap-space (<-> node-map
                                    all-nodes
                                    (filter #(#{:zero} (:used %)) <-)
                                    (map #(euclidean-distance goal-x goal-y (:x %) (:y %)) <-)
                                    (apply min <-) )]
    (+ n-steps distance-to-swap-space (Math/pow (euclidean-distance 0 0 goal-x goal-y) 2))) )

(defn print-state [{:keys [goal-x goal-y node-map]}]
  (let [goal-used (get-in node-map [goal-x goal-y :used])]
    (doseq [y (range 25)]
      (doseq [x (range 37)]
        (print (cond (#{:zero} (get-in node-map [x y :used])) "00"
                     (and (= x goal-x) (= y goal-y)) "[]"
                     (#{:too-much} (get-in node-map [x y :used])) "##"
                     :else ".." )) )
      (println) )) )

#_
(defn a-star-step [{:keys [conceived-states pq]}]
  (if-let [[{{:keys [goal-x goal-y] :as state} :state :keys [n-steps] :as a-star-node} next-pq] (pq/pq-pop pq)]
    (if (and (zero? goal-x) (zero? goal-y))
      n-steps
      (let [new-states (<-> state
                            all-valid-next-states
                            (remove conceived-states <-) )]
        {:pq (reduce pq/pq-push next-pq (map #(hash-map :state % :n-steps (inc n-steps)) new-states))
         :conceived-states (reduce conj conceived-states new-states)} ) ) ) )

(def last-printed (atom 0))

(defn a-star [{:keys [conceived-states pq]}]
  (if-let [[{{:keys [goal-x goal-y] :as state} :state :keys [n-steps] :as a-star-node} next-pq] (pq/pq-pop pq)]
    (do
      (let [current-seconds (.getEpochSecond (java.time.Instant/now))]
        (if true #_(not= current-seconds @last-printed)
          (do
            (println "goal-x" goal-x "goal-y" goal-y "n-steps" n-steps "n-conceived" (count conceived-states) "n-pq" (:count pq) "current cost" (cost-fn a-star-node))
            (print-state state)
            (Thread/sleep (long (/ 1000 20)))
            (reset! last-printed current-seconds) ) ) )
      (if (and (zero? goal-x) (zero? goal-y))
        n-steps
        (let [new-states (<-> state
                              all-valid-next-states
                              (remove conceived-states <-) )]
          (recur {:pq (reduce pq/pq-push next-pq (map #(hash-map :state % :n-steps (inc n-steps)) new-states))
                  :conceived-states (reduce conj conceived-states new-states)}) ) )) ) )

(defn part-2 []
  (a-star {:conceived-states #{initial-state}
           :pq (pq/pq-push (pq/empty-pq cost-fn) initial-a-star-node) }) )

(defn -main []
  (time (-> (part-2)
            println )) )

(ns adventofcode2016.day24
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn lines []
  (<-> "day24-input.txt"
       slurp
       str/split-lines ) )

(defn parse-lines []
  (for [[y line] (map vector (range) (lines))
        [x char] (map vector (range) line) ]
    {:x x :y y :char char} ) )

(defn initial-duct-map []
  (reduce (fn [{:keys [nodes goals] :as duct-map} {:keys [x y char] :as node}]
            {:nodes (assoc-in nodes [x y] node)
             :goals (if (not (#{\. \#} char))
                      (assoc goals (- (int char) (int \0)) node)
                      goals ) } )
          {}
          (parse-lines) ) )

(defn valid-next-xys [nodes [x y]]
  (keep (fn [[neighbor-x neighbor-y]]
          (if-let [neighbor-node (get-in nodes [neighbor-x neighbor-y])]
            (if (not= (:char neighbor-node) \#)
              [neighbor-x neighbor-y] ) ) )
        [[(inc x) y]
         [(dec x) y]
         [x (inc y)]
         [x (dec y)] ] ) )

(defn euclidean-distance [a-x a-y b-x b-y]
  (Math/pow (+ (Math/pow (- a-x b-x) 2) (Math/pow (- a-y b-y) 2)) 0.5) )

(defn cost-fn [goal-x goal-y {:keys [x y n-steps]}]
  (+ n-steps (euclidean-distance x y goal-x goal-y)) )

(defn find-distance
  ([nodes {x-a :x y-a :y :as node-a} {x-b :x y-b :y :as node-b}]
   (find-distance {:nodes nodes :goal-x x-b :goal-y y-b :visited-xys #{[x-a y-a]} :pq (pq/pq-push (pq/empty-pq (partial cost-fn x-b y-b)) {:x x-a :y y-a :n-steps 0})})  )
  ([{:keys [nodes goal-x goal-y visited-xys pq] :as state}]
   (if-let [[{:keys [n-steps] current-x :x current-y :y} next-pq] (pq/pq-pop pq)]
     (if (and (= current-x goal-x) (= current-y goal-y))
       n-steps
       (let [new-xys (<-> [current-x current-y]
                          (valid-next-xys nodes <-)
                          (remove visited-xys <-) )]
         (recur (assoc state
                       :pq (reduce pq/pq-push next-pq (map #(hash-map :x (first %) :y (second %) :n-steps (inc n-steps)) new-xys))
                       :visited-xys (reduce conj visited-xys new-xys) )) ) ) ) ) )

(defn compute-edge-weights [{:keys [nodes goals] :as duct-map}]
  (reduce (fn [edge-weights [index-a index-b]]
            (assoc-in edge-weights [index-a index-b] (find-distance nodes (goals index-a) (goals index-b))) )
          {}
          (for [index-a (range (dec (count goals)))
                index-b (range (inc index-a) (count goals)) ]
            [index-a index-b] )) )

(defn permutate [s]
  (if (= (count s) 1)
    [[(first s)]]
    (mapcat (fn [e] (map #(conj % e) (permutate (disj s e)))) s) ) )

(def part-1-traversal-sequences (map #(cons 0 %) (permutate #{1 2 3 4 5 6 7})))
(def part-2-traversal-sequences (map #(concat [0] % [0]) (permutate #{1 2 3 4 5 6 7})))

(defn part-1 []
  (let [edge-weights (<-> (initial-duct-map)
                          compute-edge-weights )]
    (<-> part-1-traversal-sequences
         (map (fn [s] (<-> s
                           (partition 2 1 <-)
                           (map #(get-in edge-weights (sort %)) <-)
                           (reduce + <-) ))
              <- )
         (apply min <-)
         println ) ) )

(defn part-2 []
  (let [edge-weights (<-> (initial-duct-map)
                          compute-edge-weights )]
    (<-> part-2-traversal-sequences
         (map (fn [s] (<-> s
                           (partition 2 1 <-)
                           (map #(get-in edge-weights (sort %)) <-)
                           (reduce + <-) ))
              <- )
         (apply min <-)
         println ) ) )

(defn -main []
  (time (part-2)) )

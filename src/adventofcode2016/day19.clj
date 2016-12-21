(ns adventofcode2016.day19
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [digest :as d]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(def puzzle-input 3014603)

(defn inc-elf [{:keys [n-elves current-elf]}]
  (mod (inc current-elf) n-elves) )

(defn find-next-elf [{:keys [giftless-elves n-elves current-elf] :as state}]
  (loop [next-elf (inc-elf state)]
    (if (giftless-elves next-elf)
      (recur (inc-elf (assoc state :current-elf next-elf)))
      next-elf ) ) )

(def part1-initial-state {:giftless-elves #{}
                          :n-elves puzzle-input
                          :current-elf 0 })

(defn part1 [{:keys [giftless-elves n-elves current-elf] :as state}]
  (let [next-elf (find-next-elf state)]
    (if (= current-elf next-elf)
      (inc current-elf)
      (recur (<-> state
                  (update <- :giftless-elves conj next-elf)
                  (assoc <- :current-elf (find-next-elf (assoc state :current-elf next-elf))) )) ) ) )

(defn p2-next-elf [elves current-elf]
  (or (first (subseq elves > current-elf))
      (first elves) ) )

(defn elvish-build-tree [min max]
  (cond (or (> min max) (< max min)) nil

        (= min max)
        {:value min :count 1 :left-child nil :right-child nil}

        :else
        (let [mid-point (+ (long (/ (- max min) 2)) min)
              left-child (elvish-build-tree min (dec mid-point))
              right-child (elvish-build-tree (inc mid-point) max) ]
          {:value mid-point
           :count (+ (if left-child (:count left-child) 0) (if right-child (:count right-child) 0) 1)
           :left-child left-child
           :right-child right-child } ) ) )

(defn elvish-count [tree]
  (:count tree) )

(defn elvish-find
  ([tree value] (elvish-find tree value []))
  ([tree value current-path]
   (cond (nil? tree) nil

         (= (:value tree) value) current-path

         :else
         (let [next-child (if (< value (:value tree)) :left-child :right-child)]
           (recur (next-child tree) value (conj current-path next-child)) ) ) ) )

(defn elvish-left-most-decendent [tree]
  (if-let [left-child (:left-child tree)]
    (recur left-child)
    tree ) )

#_
(defn elvish-right-most-parent [tree path]
  (if (= (peek path) :left-child)
    (get-in tree (pop path))
    (recur tree (pop path)) ) )

#_
(defn elvish-right-most-node? [path]
  (= (set path) #{:right-child}) )

#_
(defn elvish-next [tree value]
  (let [value-path (elvish-find tree value)
        right-child (get-in tree (conj value-path :right-child)) ]
    (cond right-child
          (elvish-left-most-decendent right-child)

          (= (peek value-path) :left-child)
          (get-in tree (pop value-path))

          (elvish-right-most-node? value-path)
          nil

          :else
          (elvish-right-most-parent tree value-path) ) ) )

(defn elvish-path->index
  ([tree path] (elvish-path->index tree path 0))
  ([tree path offset]
   (if (empty? path)
     (+ offset (if-let [left-child (:left-child tree)]
                 (:count left-child)
                 0 ))
     (let [current-child (first path)
           next-path (subvec path 1)
           next-offset (if (= current-child :right-child)
                         (+ offset (get-in tree [:left-child :count] 0) 1)
                         offset ) ]
       (recur (current-child tree) next-path next-offset) ) ) ) )

(defn elvish-nth
  ([tree index] (elvish-nth tree index 0))
  ([tree index offset]
   (let [current-index (+ (or (-> tree :left-child :count) 0) offset)]
     (cond (= index current-index) tree

           (< index current-index) (recur (:left-child tree) index offset)

           :else (recur (:right-child tree) index (+ current-index 1)) ) ) ) )

(defn elvish-nth-path
  ([tree index] (elvish-nth-path tree index 0 []))
  ([tree index offset path]
   (let [current-index (+ (or (-> tree :left-child :count) 0) offset)]
     (cond (= index current-index) path

           (< index current-index) (recur (:left-child tree) index offset (conj path :left-child))

           :else (recur (:right-child tree) index (+ current-index 1) (conj path :right-child)) ) ) ) )

(defn elvish-next [tree value]
  (<-> value
       (elvish-find tree <-)
       (elvish-path->index tree <-)
       inc
       (mod <- (:count tree))
       (elvish-nth-path tree <-)
       (get-in tree <-) ) )

(defn elvish-dec-counts [tree path]
  (if (empty? path)
    (update tree :count dec)
    (recur (<-> tree
                (update-in <- (conj path :count) dec) )
           (pop path) ) ) )

(defn elvish-delete-nth [tree index]
  (let [path-to-delete (elvish-nth-path tree index)
        node-to-delete (get-in tree path-to-delete) ]
    (cond (and (nil? (:left-child node-to-delete)) (nil? (:right-child node-to-delete)))
          (<-> tree
               (assoc-in <- path-to-delete nil)
               (elvish-dec-counts <- (pop path-to-delete)) )

          (nil? (:left-child node-to-delete))
          (let [node-to-move (:right-child node-to-delete)]
            (<-> tree
                 (assoc-in <- path-to-delete node-to-move)
                 (elvish-dec-counts <- (pop path-to-delete)) ) )

          (nil? (:right-child node-to-delete))
          (let [node-to-move (:left-child node-to-delete)]
            (if (empty? path-to-delete)
              node-to-move
              (<-> tree
                   (assoc-in <- path-to-delete node-to-move)
                   (elvish-dec-counts <- (pop path-to-delete)) ) ) )

          :else
          (let [node-to-move (elvish-left-most-decendent (get-in tree (conj path-to-delete :right-child)))
                value-to-move (:value node-to-move)
                right-child-to-move (:right-child node-to-move)
                path-to-parent-of-node-to-move (pop (elvish-find tree value-to-move)) ]
            (<-> tree
                 (assoc-in <- (conj path-to-delete :value) value-to-move)
                 (assoc-in <- (conj path-to-parent-of-node-to-move
                                    (if (= path-to-parent-of-node-to-move path-to-delete)
                                      :right-child
                                      :left-child ) )
                           right-child-to-move )
                 (elvish-dec-counts <- path-to-parent-of-node-to-move ) ) ) ) ) )

#_
(defn p2-opposite-elf [elves current-elf]
  (let [n-seats-away (long (/ (count elves) 2))
        elves-greater-than-current-elf (subseq elves > current-elf)
        n-elves-greater-than-current-elf (count elves-greater-than-current-elf) ]
    (if (<= n-seats-away n-elves-greater-than-current-elf)
      (first (drop (dec n-seats-away) elves-greater-than-current-elf))
      (first (drop (dec (- n-seats-away n-elves-greater-than-current-elf)) elves)) ) ) )

(defn part2 [{:keys [remaining-elves current-elf]}]
  (if (= (:count remaining-elves) 1)
    current-elf
    (let [current-elf-index (<-> current-elf (elvish-find remaining-elves <-) (elvish-path->index remaining-elves <-))
          index-of-elf-to-delete (mod (+ current-elf-index (long (/ (:count remaining-elves) 2))) (:count remaining-elves))
          new-remaining-elves (elvish-delete-nth remaining-elves index-of-elf-to-delete)
          next-elf (:value (elvish-next new-remaining-elves current-elf)) ]
      (recur {:remaining-elves new-remaining-elves
              :current-elf next-elf }) ) ) )

(defn -main []
  (time (<-> (part2 {:remaining-elves (elvish-build-tree 1 puzzle-input) :current-elf 1})
             println )) )

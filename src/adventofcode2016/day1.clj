(ns adventofcode2016.day1)

(def input-string "R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1, R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3, R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2, R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2, R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4, L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1")

(def directions {"N" {:turn {"L" "W" "R" "E"} :vector [ 0  1]}
                 "E" {:turn {"L" "N" "R" "S"} :vector [ 1  0]}
                 "S" {:turn {"L" "E" "R" "W"} :vector [ 0 -1]}
                 "W" {:turn {"L" "S" "R" "N"} :vector [-1  0]} })

(def visited-locations (atom #{}))
(def visited-same-location-already (atom false))

(defn parse-instruction [s]
  (let [[_ direction n-blocks-string] (re-find #"(\w)(\d+)" s)]
    {:turn direction
     :n-blocks (Long/parseLong n-blocks-string) } ) )

(defn check-for-visited-location [location]
  (when (and (not @visited-same-location-already) (@visited-locations location))
    (println location)
    (reset! visited-same-location-already true) )
  (swap! visited-locations conj location) )

(defn move [location-vector n direction-vector]
  (if (= n 0)
    location-vector
    (let [[location-x location-y] (move location-vector (dec n) direction-vector)
          [direction-x direction-y] direction-vector
          new-location [(+ location-x direction-x) (+ location-y direction-y)] ]
      (check-for-visited-location new-location)
      new-location ) ) )

(defn follow-instructions [{:keys [direction location]} {:keys [turn n-blocks]}]
  (let [new-direction (get-in directions [direction :turn turn])
        v (get-in directions [new-direction :vector])
        new-location (move location n-blocks v) ]
    {:direction new-direction :location new-location} ) )

(defn -main [& args]
  (->> input-string
       (re-seq #"\w+")
       (map parse-instruction)
       (reduce follow-instructions {:direction "N" :location [0 0]}) ) )

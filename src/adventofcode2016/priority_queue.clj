(ns adventofcode2016.priority-queue)

(defn empty-pq [key-fn]
  {:key-fn key-fn :values [] :count 0} )

(defn- pq-swap! [transient-values index-a index-b]
  (let [temp (transient-values index-a)]
    (-> transient-values
        (assoc! index-a (transient-values index-b))
        (assoc! index-b temp) ) ) )

(def ^:private pq-root-index? zero?)

(defn- pq-parent-index [index]
  (dec (long (/ (inc index) 2))) )

(defn- pq-child-indexes [index]
  [(dec (* (inc index) 2)) (dec (inc (* (inc index) 2)))] )

(defn- pq-bubble-up! [transient-values current-index]
  (if (pq-root-index? current-index)
    transient-values
    (let [parent-index (pq-parent-index current-index)
          current-key (first (transient-values current-index))
          parent-key (first (transient-values parent-index)) ]
      (if (< current-key parent-key)
        (recur (pq-swap! transient-values current-index parent-index) parent-index)
        transient-values ) ) ) )

(defn pq-push [{:keys [key-fn values count] :as pq} value]
  (let [kv [(key-fn value) value]]
    (assoc pq
           :values (-> values
                       transient
                       (assoc! count kv)
                       (pq-bubble-up! count)
                       persistent! )
           :count (inc count) ) ) )

(defn- pq-bubble-down! [transient-values count current-index]
  (let [current-key (first (transient-values current-index))
        [l-child-index r-child-index] (pq-child-indexes current-index) ]
    (cond (and (< l-child-index count) (< r-child-index count))
          (let [l-child-key (first (transient-values l-child-index))
                r-child-key (first (transient-values r-child-index)) ]
            (cond (and (<= l-child-key r-child-key) (< l-child-key current-key))
                  (recur (pq-swap! transient-values l-child-index current-index) count l-child-index)

                  (and (< r-child-key l-child-key) (< r-child-key current-key))
                  (recur (pq-swap! transient-values r-child-index current-index) count r-child-index)

                  :else transient-values ) )

          (< l-child-index count)
          (let [l-child-key (first (transient-values l-child-index))]
            (if (< l-child-key current-key)
              (pq-swap! transient-values l-child-index current-index)
              transient-values ) )

          :else transient-values ) ) )

(defn pq-pop [{:keys [values count] :as pq}]
  (let [new-count (dec count)
        return-value (second (values 0)) ]
    [return-value
     (assoc pq
            :values (-> values
                        transient
                        (assoc! 0 (values new-count))
                        (pq-bubble-down! new-count 0)
                        persistent! )
            :count new-count ) ] ) )

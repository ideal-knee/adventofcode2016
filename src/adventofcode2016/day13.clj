(ns adventofcode2016.day13
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [adventofcode2016.utils :refer [<->]]
            [adventofcode2016.priority-queue :as pq] )
  (:gen-class) )

(defn -main []
  (-> "hello-world"
      println ) )

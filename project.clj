(defproject adventofcode2016 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [digest "1.4.5"]
                 [com.taoensso/tufte "1.1.0"] ]
  :main ^:skip-aot adventofcode2016.day23
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

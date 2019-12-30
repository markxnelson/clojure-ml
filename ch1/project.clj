(defproject ch1 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [net.mikera/core.matrix "0.62.0"]]
  :main ^:skip-aot ch1.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

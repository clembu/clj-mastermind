(defproject mastermind "0.1.0-SNAPSHOT"
  :description "A smart mastermind app who tries to guess your code"
  :url "https://github.com/facelesspanda/clj-mastermind"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot mastermind.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

(defproject nedfryst "0.1.0-SNAPSHOT"
  :description "An attempt to put Clojure namespaces, including anonymous functions, under statsis."
  :url "https://github.com/hraberg/nedfryst"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.esotericsoftware.kryo/kryo "2.21"]]
  :plugins [[lein-swank "1.4.5"]]
  :manifest {"Premain-Class" "nedfryst.core"
             "Agent-Class" "nedfryst.core"}
  :aot [nedfryst.core]
  :main nedfryst.core)

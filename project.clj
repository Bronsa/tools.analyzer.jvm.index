(defproject tools.analyzer.jvm.index "0.1.0-SNAPSHOT"
  :description "Import tools.analyzer.jvm AST in Datomic"
  :url "https://www.github.com/Bronsa/tools.analyzer.jvm.index"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.analyzer.jvm "0.0.1-SNAPSHOT"]
                 [com.datomic/datomic-free "0.9.4384"]])

(ns clojure.tools.analyzer.jvm.index
  (:require [clojure.tools.analyzer.jvm.schema :as schema]
            [clojure.tools.analyzer.jvm.tx :as tx]
            [datomic.api :as d]))

(def ^:const uri "datomic:mem://tools.analyzer.jvm.index/")

(defn connect [name]
  (let [conn (d/connect
              (doto (str uri name)
                d/delete-database
                d/create-database))]
    @(d/transact conn schema/schema)
    conn))

(defn index [conn ast]
  (->> ast
      tx/transaction-data
      (d/transact conn)
      deref))

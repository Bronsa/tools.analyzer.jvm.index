(ns clojure.tools.analyzer.jvm.schema
  (:require [datomic.api :as d]
            [clojure.edn :as edn]))

(defn read-file [s]
  (edn/read-string {:readers *data-readers*}
                   (slurp s)))

(defonce schema
  (read-file "resources/schema.edn"))

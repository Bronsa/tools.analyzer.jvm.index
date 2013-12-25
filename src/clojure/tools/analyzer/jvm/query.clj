(ns clojure.tools.analyzer.jvm.query
  (:require [datomic.api :as d]))

(defn mapvcat [f coll]
  (apply concat (mapv f coll)))

(defn ast->eav [ast]
  (let [children (set (:children ast))]
    (mapvcat (fn [[k v]]
               (if (children k)
                 (if (map? v)
                   (into [[ast k v]] (ast->eav v))
                   (mapvcat (fn [v] (into [[ast k v]] (ast->eav v))) v))
                 [[ast k v]])) ast)))

(defn q [query ast]
  (d/q query (ast->eav ast)))

(comment
  (q '[:find ?var ?val
       :where
       [?let :op :let]
       [(:bindings ?let) ?bindings]
       [(count ?bindings) ?bindc]
       [(= 1 ?bindc)]
       [?let :bindings ?binding]
       [?init :op :def]
       [?init :var ?var]
       [?binding :init ?init]
       [?binding :name ?name]
       [?let :body ?body]
       [?ret :op :if]
       [?ret :then ?then]
       [?then :op :const]
       [?then :type :nil]
       [?ret :else ?else]
       [?test :op :instance-call]
       [?test :method hasRoot]
       [?test :instance ?instance]
       [?instance :op :local]
       [?instance :name ?name]
       [?else :op :def]
       [?else :var ?var]
       [?else :init ?i]
       [?i :form ?val]]
     (jvm/analyze '(defonce foo 1) (jvm/empty-env))))

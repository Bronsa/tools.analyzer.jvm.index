(ns clojure.tools.analyzer.jvm.query
  (:require [datomic.api :as d]))

(defn ast->eav [ast]
  (let [children (set (:children ast))]
    (mapcat (fn [[k v]]
               (if (children k)
                 (if (map? v)
                   (into [[ast k v]] (ast->eav v))
                   (mapcat (fn [v] (into [[ast k v]] (ast->eav v))) v))
                 [[ast k v]])) ast)))

(defn ssa [query]
  (let [[pre [_ & post]] (split-with (fn [el] (not= el :where)) query)]
    `[~@pre
      :where
      ~@(mapcat (fn [[op & rest :as form]]
                  (if (seq? op)
                    (let [[f & args] op]
                      (if (some seq? args)
                        (loop [args args to-ssa {} cur [f] binds rest ret []]
                          (if (seq args)
                            (let [[a & args] args]
                              (if (and (seq? a)
                                       (not= 'quote (first a)))
                                (let [g (gensym "?")]
                                  (recur args (assoc to-ssa g a) (conj cur g) binds ret))
                                (recur args to-ssa (conj cur a) binds ret)))
                            (let [ret (conj ret `[~(seq cur) ~@binds])]
                              (if (seq to-ssa)
                                (let [[k [f & args]] (first to-ssa)]
                                  (recur args (dissoc to-ssa k) [f] [k] ret))
                                ret))))
                        [form]))
                    [form])) post)]))

(defn q [query asts & inputs]
  (apply d/q (ssa query) (mapcat ast->eav asts) inputs))

(comment
  (q '[:find ?var ?val
       :where
       [?let :op :let]
       [(= 1 (count (:bindings ?let)))]
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
     [(jvm/analyze '(defonce foo 1) (jvm/empty-env))])

  (q '[:find ?line ?form ?name
       :where
       [?def :op :def]
       [?def :form ?form]
       [?def :env ?env]
       [(:line ?env) ?line]
       [?def :name ?name]
       [?def :init ?fn]
       [?fn :methods ?method]
       [?method :body ?body]
       [?body :statements ?statement]
       [?statement :type :string]
       [(= (first (:statements ?body)) ?statement)]]
     [(jvm/analyze '(defn x [] "foo" 1) (jvm/empty-env))]))

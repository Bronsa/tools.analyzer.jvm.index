(ns clojure.tools.analyzer.jvm.query
  (:require [datomic.api :as d]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as jvm]))

(defn ast->eav [ast]
  (let [children (set (:children ast))]
    (mapcat (fn [[k v]]
              (if (children k)
                (if (map? v)
                  (into [[ast k v]] (ast->eav v))
                  (mapcat (fn [v] (into [[ast k v]] (ast->eav v))) v))
                [[ast k v]])) ast)))

(defn ssa [{:keys [where] :as query}]
  (if-not where
    query
    (assoc query :where
           (mapcat (fn [[op & rest :as form]]
                     (if-let [[f & args] (and (seq? op) op)]
                       (if (some seq? args)
                         (loop [args args to-ssa {} cur [f] binds rest ret []]
                           (if (seq args)
                             (let [[a & args] args]
                               (if (and (seq? a)
                                        (not= 'quote (first a)))
                                 (let [g (gensym "?")]
                                   (recur args (assoc to-ssa g a) (conj cur g) binds ret))
                                 (recur args to-ssa (conj cur a) binds ret)))
                             (let [ret (conj ret (into [(seq cur)] binds))]
                               (if (seq to-ssa)
                                 (let [[k [f & args]] (first to-ssa)]
                                   (recur args (dissoc to-ssa k) [f] [k] ret))
                                 ret))))
                         [form])
                       [form])) where))))

(defn query-map [query]
  (if (map? query)
    query
    (loop [ret {:find [] :in [] :where []} query query op nil]
      (if (seq query)
        (let [[el & query] query]
          (if (keyword? el)
            (recur ret query el)
            (recur (update-in ret [op] conj el) query op)))
        (reduce-kv (fn [m k v] (if (seq v) (assoc m k v) m)) {} ret)))))

(defn resolve-calls [{:keys [where] :as query}]
  (if-not where
    query
    (assoc query :where
           (mapv (fn [[op & rest :as form]]
                   (if-let [[f & args] (and (seq? op) op)]
                     (if-let [f-var (and (symbol? f) (resolve f))]
                       (into [(seq (into [(symbol (str (ns-name (.ns f-var)))
                                                  (str (.sym f-var)))] args))]
                             rest)
                       form)
                     form)) where))))

(defn idx-many [ast]
  (ast/postwalk ast
                (fn [{:keys [children] :as ast}]
                  (merge ast
                         (reduce (fn [m c]
                                   (let [v (c ast)
                                         v (if (vector? v)
                                             (mapv (fn [x i] (assoc x :idx i ))
                                                   v (range))
                                             v)]
                                     (assoc m c v))) {} children)))))

(defn db [asts]
  (mapcat (fn [ast] (-> ast idx-many ast->eav)) asts))

(defn prepare [query]
  (-> query query-map ssa resolve-calls))

(defn q [query asts & inputs]
  (apply d/q (prepare query) (db asts) inputs))

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
       [(-> ?def :env :line) ?line]
       [?def :name ?name]
       [?def :init ?fn]
       [?fn :methods ?method]
       [?method :body ?body]
       [?body :statements ?statement]
       [?statement :type :string]
       [?statement :idx 0]]
     [(jvm/analyze '(defn x [] "foo" 1) (jvm/empty-env))]))

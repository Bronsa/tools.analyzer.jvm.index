(ns clojure.tools.analyzer.jvm.tx
  (:require [datomic.api :as d]))

(defmulti -tx-data :op)

(defn tx-data [{:keys [env form children tag ret-tag bind-tag return-tag] :as ast}]
  (merge (-tx-data ast)
         {:ast/namespace (-> env :ns str)
          :ast/form      (binding [*print-dup* true]
                           (pr-str form))
          :ast/context   (-> env :context {:return    :ast.context/return
                                          :expr      :ast.context/expr
                                          :statement :ast.context/statement})}
         (when children
           {:ast/children  children})
         (when-let [line (-> env :line)]
           {:ast/line line})
         (when-let [column (-> env :column)]
           {:ast/column column})
         (when-let [file (-> env :file)]
           {:ast/file (str file)})
         (when tag
           {:ast/tag (str tag)})
         (when bind-tag
           {:ast/bind-tag (str bind-tag)})
         (when ret-tag
           {:ast/ret-tag (str ret-tag)})
         (when return-tag
           {:ast/return-tag (str return-tag)})))

(defn indexed-tx-data [nodes]
  (vec (map-indexed (fn [i node]
                      (assoc (tx-data node)
                        :ast/idx i))
                    nodes)))

(defmethod -tx-data :binding
  [{:keys [init name local arg-id variadic? mutable] :or {variadic? ::not-present}}]
  (merge {:ast/op     :ast.op/binding
          :ast/name   (str name)
          :local/type (-> local {:catch :local.type/catch
                                :let   :local.type/let
                                :loop  :local.type/loop
                                :letfn :local.type/letfn
                                :arg   :local.type/arg
                                :fn    :local.type/fn
                                :field :local.type/field})}
         (when init
           {:ast/init (tx-data init)})
         (when arg-id
           {:arg/id arg-id})
         (when-not (::not-present variadic?)
           {:ast/variadic? variadic?})
         (when mutable
           {:field/mutable mutable})))

(defmethod -tx-data :catch
  [{:keys [local body class]}]
  {:ast/op    :ast.op/catch
   :ast/local (tx-data local)
   :ast/body  (tx-data body)
   :ast/class (str class)})

(defmethod -tx-data :case
  [{:keys [test default tests thens shift mask low high switch-type test-type skip-check?]}]
  {:ast/op           :ast.op/case
   :ast/test         (tx-data test)
   :case/default     (tx-data default)
   :case/tests       (mapv tx-data tests)
   :case/thens       (mapv tx-data thens)
   :case/shift       shift
   :case/mask        mask
   :case/low         low
   :case/high        high
   :case/switch-type switch-type
   :case/test-type   test-type
   :case/skip-check? (or skip-check? false)})

(defmethod -tx-data :case-test
  [{:keys [test hash]}]
  {:ast/op   :ast.op/case-test
   :ast/test (tx-data test)
   :ast/hash hash})

(defmethod -tx-data :case-then
  [{:keys [then hash]}]
  {:ast/op   :ast.op/case-then
   :ast/then (tx-data then)
   :ast/hash hash})

(defmethod -tx-data :const
  [{:keys [meta type val]}]
  (merge {:ast/op       :ast.op/const
          :ast/type     type
          :ast/val      (binding [*print-dup* true]
                          (pr-str val))
          :ast/literal? true}
         (when
             {:ast/meta meta})))

(defmethod -tx-data :def
  [{:keys [meta name init var doc]}]
  (merge {:ast/op   :ast.op/def
          :ast/name (str name)
          :ast/var  (binding [*print-dup* true]
                      (pr-str var))}
         (when meta
           {:ast/meta (tx-data meta)})
         (when doc
           {:ast.def/doc doc})
         (when init
           {:ast/init (tx-data init)})))

(defmethod -tx-data :deftype
  [{:keys [name class-name fields methods interfaces]}]
  {:ast/op         :ast.op/deftype
   :ast/name       (str name)
   :ast/class      (str class-name)
   :ast/fields     (indexed-tx-data fields)
   :ast/methods    (mapv tx-data methods)
   :ast/interfaces (mapv tx-data interfaces)})

(defmethod -tx-data :do
  [{:keys [statements ret]}]
  {:ast/op         :ast.op/do
   :ast/statements (indexed-tx-data statements)
   :ast/ret        (tx-data ret)})

(defmethod -tx-data :fn
  [{:keys [local methods name variadic? max-fixed-arity]}]
  (merge {:ast/op             :ast.op/fn
          :ast/name           (str name)
          :ast/variadic?      variadic?
          :fn/max-fixed-arity max-fixed-arity
          :ast/methods        (mapv tx-data methods)}
         (when local
           {:ast/local (tx-data local)})))

(defmethod -tx-data :fn-method
  [{:keys [loop-id variadic? params fixed-arity body]}]
  {:ast/op          :ast.op/fn-method
   :loop/id         loop-id
   :ast/variadic?   variadic?
   :ast/fixed-arity fixed-arity
   :ast/body        (tx-data body)
   :ast/params      (indexed-tx-data params)})

(defmethod -tx-data :host-interop
  [{:keys [target m-or-f]}]
  {:ast/op     :ast.op/host-interop
   :ast/target (tx-data target)
   :ast/m-or-f (str m-or-f)})

(defmethod -tx-data :if
  [{:keys [test then else]}]
  {:ast/op   :ast.op/if
   :ast/test (tx-data test)
   :ast/then (tx-data then)
   :ast/else (tx-data else)})

(defmethod -tx-data :import
  [{:keys [class]}]
  {:ast/op    :ast.op/import
   :ast/class (str class)})

(defmethod -tx-data :instance?
  [{:keys [target class]}]
  {:ast/op     :ast.op/instance?
   :ast/class  (str class)
   :ast/target (tx-data target)})

(defmethod -tx-data :instance-call
  [{:keys [instance args validated? class method]}]
  (merge {:ast/op       :ast.op/instance-call
          :ast/instance (tx-data instance)
          :ast/method   (str method)}
         (when validated?
           {:ast/validated? true
            :ast/class      (str class)})
         (when args
           {:ast/args (indexed-tx-data args)})))

(defmethod -tx-data :instance-field
  [{:keys [instance field class assignable?]}]
  (merge {:ast/op       :ast.op/instance-field
          :ast/instance (tx-data instance)
          :ast/class    (str class)
          :ast/field    (str field)}
         (when assignable?
           {:ast/assignable? true})))

(defmethod -tx-data :invoke
  [{:keys [fn args meta]}]
  (merge {:ast/op   :ast.op/invoke
          :ast/fn   (tx-data fn)
          :ast/args (indexed-tx-data args)}
         (when meta
           {:ast/meta meta})))

(defmethod -tx-data :keyword-invoke
  [{:keys [fn args]}]
  {:ast/op   :ast.op/keyword-invoke
   :ast/fn   (tx-data fn)
   :ast/args (indexed-tx-data args)})

(defmethod -tx-data :let
  [{:keys [bindings body]}]
  {:ast/op       :ast.op/let
   :ast/bindings (indexed-tx-data bindings)
   :ast/body     (tx-data body)})

(defmethod -tx-data :letfn
  [{:keys [bindings body]}]
  {:ast/op       :ast.op/let
   :ast/bindings (indexed-tx-data bindings)
   :ast/body     (tx-data body)})

(defmethod -tx-data :local
  [{:keys [name local arg-id variadic? mutable assignable?]
    :or {variadic? ::not-present}}]
  (merge {:ast/op     :ast.op/local
          :ast/name   (str name)
          :local/type (-> local {:catch :local.type/catch
                                :let   :local.type/let
                                :loop  :local.type/loop
                                :letfn :local.type/letfn
                                :arg   :local.type/arg
                                :fn    :local.type/fn
                                :field :local.type/field})}
         (when arg-id
           {:arg/id arg-id})
         (when-not (::not-present variadic?)
           {:ast/variadic? variadic?})
         (when mutable
           {:field/mutable   mutable
            :ast/assignable? assignable?})))

(defmethod -tx-data :loop
  [{:keys [bindings body loop-id]}]
  {:ast/op       :ast.op/loop
   :ast/bindings (indexed-tx-data bindings)
   :ast/body     (tx-data body)
   :loop/id      loop-id})

(defmethod -tx-data :map
  [{:keys [keys vals]}]
  {:ast/op   :ast.op/map
   :ast/keys (indexed-tx-data keys)
   :ast/vals (indexed-tx-data vals)})

(defmethod -tx-data :method
  [{:keys [loop-id this params name fixed-arity body]}]
  {:ast/op          :ast.op/fn-method
   :loop/id         loop-id
   :ast/this        (tx-data this)
   :ast/name        (str name)
   :ast/fixed-arity fixed-arity
   :ast/body        (tx-data body)
   :ast/params      (indexed-tx-data params)})

(defmethod -tx-data :monitor-enter
  [{:keys [target]}]
  {:ast/op     :ast.op/monitor-enter
   :ast/target (tx-data target)})

(defmethod -tx-data :monitor-exit
  [{:keys [target]}]
  {:ast/op     :ast.op/monitor-exit
   :ast/target (tx-data target)})

(defmethod -tx-data :new
  [{:keys [args class validated?]}]
  (merge {:ast/op    :ast.op/new
          :ast/class (str class)
          :ast/args  (indexed-tx-data args)}
         (when validated?
           {:ast/validated? true})))

(defmethod -tx-data :prim-invoke
  [{:keys [fn args meta prim-interface arg-tags]}]
  (merge {:ast/op             :ast.op/prim-invoke
          :ast/fn             (tx-data fn)
          :ast/args           (indexed-tx-data args)
          :ast/arg-tags       (mapv str arg-tags)
          :ast/prim-interface (str prim-interface)}
         (when meta
           {:ast/meta meta})))

(defmethod -tx-data :protocol-invoke
  [{:keys [fn args meta]}]
  (merge {:ast/op   :ast.op/protocol-invoke
          :ast/fn   (tx-data fn)
          :ast/args (indexed-tx-data args)}
         (when meta
           {:ast/meta meta})))

(defmethod -tx-data :quote
  [{:keys [expr]}]
  {:ast/op       :ast.op/quote
   :ast/expr     (tx-data expr)
   :ast/literal? true})

(defmethod -tx-data :recur
  [{:keys [exprs loop-id]}]
  {:ast/op    :ast.op/recur
   :loop/id   loop-id
   :ast/exprs (indexed-tx-data exprs)})

(defmethod -tx-data :reify
  [{:keys [class-name methods interfaces]}]
  {:ast/op         :ast.op/reify
   :ast/class      (str class-name)
   :ast/methods    (mapv tx-data methods)
   :ast/interfaces (mapv tx-data interfaces)})

(defmethod -tx-data :set
  [{:keys [items]}]
  {:ast/op    :ast.op/set
   :ast/items (indexed-tx-data items)})

(defmethod -tx-data :set!
  [{:keys [target val]}]
  {:ast/op     :ast.op/set!
   :ast/target (tx-data target)
   :ast/val    (tx-data val)})

(defmethod -tx-data :static-call
  [{:keys [args method validated? class]}]
  (merge {:ast/op       :ast.op/static-call
          :ast/method   (str method)}
         (when validated?
           {:ast/validated? true
            :ast/class      (str class)})
         (when args
           {:ast/args (indexed-tx-data args)})))

(defmethod -tx-data :static-field
  [{:keys [instance field class assignable?]}]
  (merge {:ast/op       :ast.op/static-field
          :ast/class    (str class)
          :ast/field    (str field)}
         (when assignable?
           {:ast/assignable? true})))

(defmethod -tx-data :the-var
  [{:keys [var]}]
  {:ast/op :ast.op/the-var
   :ast/var (binding [*print-dup* true]
              (pr-str var))})

(defmethod -tx-data :vector
  [{:keys [items]}]
  {:ast/op    :ast.op/vector
   :ast/items (indexed-tx-data items)})

(defmethod -tx-data :with-meta
  [{:keys [meta expr]}]
  {:ast/op   :ast.op/with-meta
   :ast/meta (tx-data meta)
   :ast/expr (tx-data expr)})

(defn transaction-data
  [ast]
  [(assoc (tx-data ast)
      :db/id          (d/tempid :db.part/user)
      :ast/top-level? true)])

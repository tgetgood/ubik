(ns ubik.db
  (:require [datomic.api :as d]))

(def clojure-schema
  "A (partial) schema for clojure data itself."
  [
   ;; Clojure data types (partial list))

   ;; This enum of types only serves to simplify reconstructing forms from the
   ;; database. The type of an entity is obvious from the shared namespace of
   ;; its attributes, but that's hard to dispatch on.

   {:db/ident       :form/type
    :db/valueType   :db.type/ref
    :db/isComponent true
    :db/index       true
    :db/cardinality :db.cardinality/one
    :db/doc         "Clojure data type as opposed to datomic schema type."}

   {:db/ident :type.number/long}
   {:db/ident :type.number/double}
   {:db/ident :type/string}
   {:db/ident :type/keyword}
   {:db/ident :type/boolean}
   {:db/ident :type/symbol}
   {:db/ident :type/nil}
   {:db/ident :type/regex}
   {:db/ident :type/list}
   {:db/ident :type/vector}
   {:db/ident :type/set}
   {:db/ident :type/map}

   ;; Single DB entity for all nils.
   {:db/ident :type.singleton/nil}

   ;; Atomic values

   {:db/ident       :long/value
    :db/doc         "Value of a Long"
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/long}

   {:db/ident       :double/value
    :db/doc         "Value of a Double"
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/double}

   {:db/ident       :keyword/value
    :db/doc         "Value of a Keyword"
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/keyword}

   {:db/ident       :string/value
    :db/doc         "Value of a String"
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/string}

   {:db/ident       :boolean/value
    :db/doc         "value of a bool"
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/boolean}

   {:db/ident       :symbol/value
    :db/doc         "Name of a Symbol"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/string}

   {:db/ident       :regex/value
    :db/doc         "Regex pattern"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/string}

   ;; Collections

   {:db/ident       :set/element
    :db/doc         "An element of a set"
    :db/cardinality :db.cardinality/many
    :db/valueType   :db.type/ref}

   {:db/ident       :list/head
    :db/doc         "First element of a list"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/ref}

   {:db/ident       :list/tail
    :db/doc         "Rest of a list"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/ref}

   {:db/ident       :vector/element
    :db/doc         "Element of a vector"
    :db/cardinality :db.cardinality/many
    :db/valueType   :db.type/ref}

   {:db/ident       :vector.element/index
    :db/doc         "Vector index of an element"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/long}

   {:db/ident       :vector.element/value
    :db/doc         "Value stored in a vector element"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/ref}

   {:db/ident       :map/element
    :db/doc         "Key value pair"
    :db/cardinality :db.cardinality/many
    :db/valueType   :db.type/ref}

   {:db/ident       :map.element/key
    :db/doc         "A map key"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/ref}

   {:db/ident       :map.element/value
    :db/doc         "A map value"
    :db/cardinality :db.cardinality/one
    :db/valueType   :db.type/ref}])

(def codebase-schema
  [
   ;; Code snippet
   {:db/ident       :code/form
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc         "code itself"}

   ;; This should link the editor to the form being edited.
   {:db/ident       :code/raw-text
    :db/valueType   :db.type/string
    :db/isComponent true
    :db/fulltext    true
    :db/cardinality :db.cardinality/one
    :db/doc         "Text of code, may or may not be valid."}

   {:db/ident       :code/previous
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc         "Other code blocks from which this was derived."}


   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Super sloppy dev stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db-uri "datomic:mem://demo")

(def conn nil)

(defn reset-db! []
  "Deletes DB, recreates it and adds schema. Only use for early dev."
  (d/delete-database db-uri)
  (d/create-database db-uri)
  (let [c (d/connect db-uri)]
    (d/transact c clojure-schema)
    (d/transact c codebase-schema)
    (alter-var-root #'conn (constantly c))))

(reset-db!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Querying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clj-pull
  '[:form/type
    :db/id
    :long/value
    :double/value
    :string/value
    :keyword/value
    :symbol/value
    :boolean/value
    :vector.element/index
    {:set/element          ...
     :list/head            ...
     :list/tail            ...
     :vector/element       ...
     :vector.element/value ...
     :map/element          ...
     :map.element/key      ...
     :map.element/value    ...}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; DB <-> In memory data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Datomify
  (tx [this tempid]))

(declare pull)

(defn datomify
  "Return Datomic tx object for given clojure data. Tempid of data is attached
  as metadata. "
  [data]
  ;; Structural sharing. This feels ludicrous, but so far it's working.
  (if (some-> data
              meta
              ::id
              pull
              (= data))
    {:db/id (::id (meta data))}
    (let [id (str (gensym))]
      (with-meta (tx data id) {:tempid id}))))

(extend-protocol Datomify
  java.lang.Long
  (tx [v id]
    {:db/id      id
     :form/type  :type.number/long
     :long/value v})

  java.lang.Double
  (tx [v id]
    {:db/id        id
     :form/type    :type.number/double
     :double/value v})

  java.lang.String
  (tx [v id]
    {:db/id        id
     :form/type    :type/string
     :string/value v})

  java.lang.Boolean
  (tx [v id]
    {:db/id         id
     :form/type     :type/boolean
     :boolean/value v})

  java.util.regex.Pattern
  (tx [v id]
    {:db/id        id
     :form/type    :type/regex
     :regex/value (str v)})

  clojure.lang.Symbol
  (tx [v id]
    {:db/id        id
     :form/type    :type/symbol
     :symbol/value (str v)})

  clojure.lang.Keyword
  (tx [v id]
    {:db/id         id
     :form/type     :type/keyword
     :keyword/value v})

  clojure.lang.PersistentHashSet
  (tx [v id]
    {:db/id       id
     :form/type   :type/set
     :set/element (map datomify v)})

  clojure.lang.PersistentVector
  (tx [v id]
    {:db/id     id
     :form/type :type/vector
     :vector/element
     (map-indexed (fn [i x]
                    {:db/id                (d/tempid :db.part/user)
                     :vector.element/index i
                     :vector.element/value (datomify x)})
                  v)})

  clojure.lang.PersistentHashMap
  (tx [v id]
    {:db/id       id
     :form/type   :type/map
     :map/element (map (fn [[k v]]
                         {:db/id             (d/tempid :db.part/user)
                          :map.element/key   (datomify k)
                          :map.element/value (datomify v)})
                       v)})

  ;; FIXME: Identical implementations... use extend
  clojure.lang.PersistentArrayMap
  (tx [v id]
    {:db/id       id
     :form/type   :type/map
     :map/element (map (fn [[k v]]
                         {:db/id             (d/tempid :db.part/user)
                          :map.element/key   (datomify k)
                          :map.element/value (datomify v)})
                       v)})

  clojure.lang.PersistentList
  (tx [[h & t] id]
    (merge {:db/id     id
            :form/type :type/list
            :list/head (datomify h)}
           (when t
             {:list/tail (datomify t)})))

  clojure.lang.Cons
  (tx [[h & t] id]
    (merge {:db/id     id
            :form/type :type/list
            :list/head (datomify h)}
           (when t
             {:list/tail (datomify t)})))

  nil
  (tx [_ _]
    {:db/id     :type.singleton/nil
     :form/type :type/nil}))

(defmulti clojurise (fn [x] (-> x :form/type :db/ident)))

(defmethod clojurise :type.number/long
  [{:keys [long/value]}]
  value)

(defmethod clojurise :type.number/double
  [{:keys [double/value]}]
  value)

(defmethod clojurise :type/string
  [{:keys [string/value]}]
  value)

(defmethod clojurise :type/keyword
  [{:keys [keyword/value]}]
  value)

(defmethod clojurise :type/boolean
  [{:keys [boolean/value]}]
  value)

(defmethod clojurise :type/symbol
  [{:keys [symbol/value db/id]}]
  (with-meta (symbol value) {::id id}))

(defmethod clojurise :type/nil
  [_]
  nil)

(defmethod clojurise :type/set
  [{:keys [set/element db/id]}]
  (with-meta
    (into #{} (map clojurise element))
    {::id id}))

(defmethod clojurise :type/list
  [{:keys [list/head list/tail db/id]}]
  (with-meta
    (conj (when tail (clojurise tail)) (clojurise head))
    {::id id}))

(defmethod clojurise :type/vector
  [{:keys [vector/element db/id]}]
  (with-meta
    (into [] (comp (map :vector.element/value) (map clojurise))
          (sort-by :vector.element/index element))
    {::id id}))

(defmethod clojurise :type/map
  [{:keys [map/element db/id]}]
  (with-meta
    (into {} (map (fn [{:keys [:map.element/key :map.element/value]}]
                    [(clojurise key) (clojurise value)]))
          element)
    {::id id}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn push [data]
  (let [tx-data (datomify data)
        tx @(d/transact conn [tx-data])]
    (get-in tx [:tempids (:tempid (meta tx-data))])))

(defn pull [eid]
  (when-let [res (d/pull (d/db conn) clj-pull eid)]
    (clojurise res)))

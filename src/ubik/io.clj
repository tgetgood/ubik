(ns ubik.io
  (:require [clojure.datafy :refer [datafy]]
            [datomic.api :as d]
            [ubik.db :as db]))

(defprotocol Datomify
  (tx [this tempid]))

(defn datomify
  "Return Datomic tx object for given clojure data. Tempid of data is attached
  as metadata."
  [data]
  (if-let [eid (::id (meta data))]
    (with-meta {:db/id eid} (meta data))
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
    {:db/ident  :type/nil
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
;; Read-write data to datomic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def conn db/conn)

(defn save-to-datomic! [data]
  (let [tx-data (datomify data)
        tx @(d/transact conn [tx-data])]
    (get-in tx [:tempids (:tempid (meta tx-data))])))

(defn pull-from-datomic! [eid]
  (when-let [res (d/pull (d/db conn) '[*] eid)]
    (clojurise res)))

(defn builtin? [x]
  (contains? (:publics (datafy (the-ns 'clojure.core))) x))

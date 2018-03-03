(ns ubik.interactive.core
  (:refer-clojure :exclude [find -deref])
  #?@(:clj
       [(:require
         [clojure.walk :as walk]
         [ubik.core :as core]
         [ubik.geometry :as geo]
         [ubik.interactive.caching :as cache]
         [ubik.interactive.db :as db])]
       :cljs
       [(:require
         [ubik.core :as core]
         [ubik.geometry :as geo]
         [ubik.interactive.db :as db])]))

(defprotocol ISubscription
  (lookup [this signal-graph]))

(deftype SubscribedShape [key body
                          ^:volatile-mutable _args
                          ^:volatile-mutable _shape]
  ISubscription
  (lookup [_ sg]
    #_(let [args (map #(% sg) subscriptions)]
      (if (= args _args)
        _shape
        (let [shape (instantiate (apply render-fn args) sg)]
          (set! _args args)
          (set! _shape shape)
          shape)))))

(def symbol-table (atom {}))

(defn sub [kw]
  (list 'sub kw)
  #_(when-not (contains? @symbol-table kw)
    (swap! symbol-table ))
  )

#?(:clj
   ;; REVIEW: Better to have a record with a signal protocol or some such...
   (defn subscription? [form]
     (and (list? form)
          (= (count form) 2)
          (= 'sub (first form))
          (keyword? (second form)))))

#?(:clj
   (defn intern-subscription [form table]
     (let [k (second form)]
       (if (contains? @table k)
         (get @table k)
         (let [sym (gensym)]
           (swap! table assoc k sym)
           sym)))))

#?(:clj
   (defn create-subscription
     "Returns a subscription for form."
     [form]
     (let [symbol-table (atom {})

           body         (walk/prewalk
                         (fn [f]
                           (if (subscription? f)
                             (intern-subscription f symbol-table)
                             f))
                         form)

           sym-seq      (seq @symbol-table)

           sub-fn       `(fn [~@(map val sym-seq)]
                           ~body)

           memoize?     (not (:no-cache (meta form)))

           memo-fn      (if memoize?
                          `(cache/cached-fn ~sub-fn)
                          sub-fn)

           sg           (gensym)]

       `(fn [~sg]
          (~memo-fn ~@(map (fn [s]
                             `((get ~sg ~s) ~sg))
                           (map key sym-seq)))))))

#?(:clj
   (defmacro defsubs [name m]
     `(def ~name
        ~(into {}
               (map (fn [[k# v#]] [k# (create-subscription v#)]) m)))))

(defsubs ex
  {:current (:current (sub :db))
   :view (nth (:examples (sub :db)) (sub :current))
   :window ^:no-cache (:window (sub :db))
   :window-height (:height (sub :window))
   })

(defn instantiate [shape state]
  (core/walk-down shape #(lookup % state)))

(defn subscribed-shape
  {:style/indent [1 :form]}
  [subscriptions render-fn]
  #_(SubscribedShape. subscriptions render-fn (gensym "no-match") nil))

(defprotocol MyDeref
  (-deref [this db]))

(defonce world (atom nil))

(defrecord World [shape]
  MyDeref
  (-deref [_ db]
    (instantiate shape db)))

(defn halucination [shape]
  (World. shape))

(defn reality [w]
  (reset! world (-deref w @@db/app-db)))

(defn sub-tagged? [s t]
  (cond
    (contains? (core/get-tags s) t) true
    (sequential? s)                 (some #(sub-tagged? % t) s)
    (core/has-children? s)          (recur (core/children s) t)
    :else                           false))

(defn find [tag location]
  (->> @world
       (geo/effected-branches location)
       (filter #(sub-tagged? % tag))
       first))

(defn tagged-value [shape tag]
  (if-let [v (core/get-tag-data shape tag)]
    v
    (cond
      (sequential? shape) (first (map #(tagged-value % tag) shape))
      (core/has-children? shape) (recur (core/children shape) tag)
      :else nil)))

(defn lookup-tag [tag location]
  (tagged-value (find tag location) tag))

(require '[clojure.pprint :refer [pp pprint]])

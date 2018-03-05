(ns ubik.interactive.core
  (:refer-clojure :exclude [find -deref])
  (:require [clojure.walk :as walk]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.caching :as cache]
            [ubik.interactive.db :as db]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Signal
  (-value [this signal-graph]))

;; TODO: Eventually we'll want more aggressive caching.
(deftype MemoizedSubscription [])

(deftype SimpleSubscription [dependencies reaction
                             ^:volatile-mutable _last-args
                             ^:volatile-mutable _last-val]
  Signal
  (-value [_ sg]
    (let [args (->> dependencies (map #(get sg %)) (map #(-value % sg)))]
      (if (= _last-args args)
        _last-val
        (let [next-val (apply reaction args)]
          (set! _last-args args)
          (set! _last-val next-val)
          next-val)))))

(defrecord RefSub [ref]
  Signal
  (-value [_ _] @ref))

(def db (RefSub. db/app-db))

(defn subscription? [sig]
  (satisfies? Signal sig))

(defn subscription
  {:style/indent [1 :form]}
  [deps reaction]
  (SimpleSubscription. deps reaction (gensym "NOMATCH") nil))

(defn sub
  {:style/indent [1 :form]
   :doc "Alias for subscription (or subscribe)."}
  [deps reaction]
  (subscription deps reaction))

(defn deref-signal
  "Returns the current value of a signal"
  [sig graph]
  (cond
    (keyword? sig) (-value (get graph sig) graph)
    (subscription? sig)  (-value sig graph)
    ;; TODO: Error logging
    :else          nil))

#?(:clj
   (defn intern-subscription [form table]
     (let [k (second form)
           tv @table]
       (if (contains? tv k)
         (get tv k)
         (let [sym (gensym)]
           (if (compare-and-set! table tv (assoc tv k sym))
             sym
             (recur form table)))))))

;; REVIEW: Here's an idea: Pass in the subscription magic marker to the
;; macro. That would be interesting. It makes it explicit that I want to use
;; (SYM :key) to access other subscriptions. Likely very confusing for new
;; users.
;;
;; At the same time we should provide a re-frame style functional form
;; (subscription [:k1 :k2] (fn [k1 k2] ...)) which is very transparent and easy
;; to use.
;;
;; Subscriptions should probably also be a type, because this is getting a
;; little silly.
#?(:clj
   (defn build-subscription
     "Returns a subscribed version of form.

  This subscription is a function which given a signal graph returns a value.

  The signal graph is just a map from keys to subscriptions.

  A subscription does not need to be part of the signal graph it receives (but
  probably will be). Recursive calls will end in disaster.

  By default subscriptions are memoized so that recomputation is only necessary
  if their upstream subscriptions take on a new value. ^no-cache metadata on
  form will prevent memoization, as will a subscription to the :db sub. This
  last is to prevent massive memory consumption. It might make sense to add a
  ^force-cache metadata as well.

  Even if the subscription isn't fully memoised, the last value is cached so
  checks are quick if nothing has changed."
     [subscription? form]
     (let [symbol-table (atom {})

           body         (walk/prewalk
                         (fn [f]
                           (if (subscription? f)
                             (intern-subscription f symbol-table)
                             f))
                         form)

           sym-seq      (seq @symbol-table)]
       `(subscription [~@(map key sym-seq)]
          (fn [~@(map val sym-seq)]
            ~body)))))

#?(:clj
   (defn sub-checker [sym]
     (fn [form]
       (and (list? form)
            (= (count form) 2)
            (symbol? (first form))
            (= (first form) sym)
            (keyword? (second form))))))

#?(:clj
   (defmacro defsubs [name operator sub-map]
     (let [sub? (sub-checker operator)]
       `(def ~name
          ~(into {}
                 (map (fn [[k# v#]] [k# (build-subscription sub? v#)])
                      sub-map))))))

#?(:clj
   (defmacro sub++
     "Helper macro to build subscriptions quickly. Possibly too
  slick. Definitely a little ugly.

  Walks form and collects all forms (sub :??) where :?? can be any subscription
  key. Build a subscription by replacing these sub forms with symbol and
  wrapping in a function. "
     ;; TODO: Example
     [form]
     (build-subscription (sub-checker 'sub) form)))

(defn walk-subscriptions
  [shape sg]
  (cond
    (subscription? shape)
    (recur (deref-signal shape sg) sg)

    (core/has-children? shape)
    (core/update-children shape #(walk-subscriptions % sg))

    :else
    shape))

(defn realise-world
  "Returns the passed in shape with all subscriptions replaced by their
  current values."
  [shape subs]
  (walk-subscriptions shape (assoc subs :db db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal Bookkeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sub-tagged? [s t]
  (cond
    (contains? (core/get-tags s) t) true
    (sequential? s)                 (some #(sub-tagged? % t) s)
    (core/has-children? s)          (recur (core/children s) t)
    :else                           false))

#_(defn find [tag location]
  (->> @world
       (geo/effected-branches location)
       (filter #(sub-tagged? % tag))
       first))

;; (defn tagged-value [shape tag]
;;   (if-let [v (core/get-tag-data shape tag)]
;;     v
;;     (cond
;;       (sequential? shape) (first (map #(tagged-value % tag) shape))
;;       (core/has-children? shape) (recur (core/children shape) tag)
;;       :else nil)))

;; (defn lookup-tag [tag location]
;;   (tagged-value (find tag location) tag))

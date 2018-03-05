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

(defn sub
  "Returns a subscription to the value k."
  [k]
  ^::subscription
  (fn [signal-graph]
    ((get signal-graph k) signal-graph)))

(defn subscription? [form]
  #?(:clj
     (or (::subscription (meta form))
         ;; This might be overkill, but I'm worried about macromagical
         ;; extravagerrors.
        (and (list? form)
             (= (count form) 2)
             (symbol? (first form))
             (= (resolve (first form)) (var sub))
             (keyword? (second form))))
     :cljs (::subscription (meta form))))

#?(:clj
   (defn intern-subscription [form table]
     (let [k (second form)]
       (if (contains? @table k)
         (get @table k)
         (let [sym (gensym)]
           (swap! table assoc k sym)
           sym)))))

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
   (defn create-subscription
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

           ;; Don't memoize subscriptions to the entire DB. There's not much
           ;; point since the DB as a whole is going to be in constant flux. The
           ;; savings are to be had downstream.
           ;;
           ;; Incidentally, this is the real reason that you should have a bunch
           ;; of trivial seeming key lookup subscriptions.
           memoize?     (or (contains? @symbol-table :db)
                            (not (:no-cache (meta form))))

           memo-fn      (if memoize?
                          `(cache/cached-fn ~sub-fn)
                          `(cache/cache-1 ~sub-fn))

           sg           (gensym)]

       ;; We need this closure so that the memoized function doesn't get
       ;; recreated.
       `(let [f# ~memo-fn]
          (fn [~sg]
            (f# ~@(map (fn [s]
                         `((get ~sg ~s) ~sg))
                       (map key sym-seq))))))))

#?(:clj
   (defmacro defsubs [name m]
     `(def ~name
        ~(into {}
               (map (fn [[k# v#]] [k# (create-subscription v#)]) m)))))

(defn log [m]
  ;; TODO: Logging solution.
  (println m))

(defn- check-key [m k]
  (cond
    (= k :db)
    (log
     "You're clobbering the :db subscription. Things won't go well for you.")

    (contains? m k)
    (log (str "Multiple definitions of " k
              " found. This will probably end badly."))
    :else nil))

(defn merge-sub-maps
  "Merges all subscription maps. Logs an error if any keys conflict or the
  special :db key is overwritten"
  ([m1 m2]
   (reduce (fn [m [k v]]
             (check-key m k)
             (assoc m k v))
           m1 m2))
  ([m1 m2 & more]
   (reduce merge-sub-maps (merge-sub-maps m1 m2) more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Stateful Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

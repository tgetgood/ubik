(ns ubik.interactive.core
  (:refer-clojure :exclude [find -deref])
  (:require [ubik.core :as core]
            [ubik.interactive.db :as db]
            [ubik.geometry :as geo]))

(defprotocol ISubscription
  (realise [this signal-graph]))

(defn instantiate [shape state]
  (core/walk-down shape #(realise % state)))

(extend-protocol ISubscription
  ;; If you don't subscribe, this is pass through. You can alternately think of
  ;; it as saying everything subscribes, and by default the subscription is
  ;; trivial.
  #?(:cljs default :clj Object)
  (realise [this _] this))

(deftype SubscribedShape [subscriptions render-fn
                          ^:volatile-mutable _args ^:volatile-mutable _shape]
  ISubscription
  ;; TODO: this is just slovenly. Works atm, but we definitely need a real
  ;; signal graph eventually.
  (realise [_ sg]
    (let [args (map #(% sg) subscriptions)]
      (if (= args _args)
        _shape
        (let [shape (instantiate (apply render-fn args) sg)]
          (set! _args args)
          (set! _shape shape)
          shape)))))

(defn subscribed-shape
  {:style/indent [1 :form]}
  [subscriptions render-fn]
  (SubscribedShape. subscriptions render-fn (gensym "no-match") nil))

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

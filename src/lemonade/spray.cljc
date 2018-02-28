(ns lemonade.spray
  (:require [clojure.walk :as walk]
            [lemonade.core :as core]))

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

(ns ubik.interactive.events.quil)

(def no-fn (constantly nil))

(defn event-map
  [host dispatch-fn]
  {:key-pressed    no-fn
   :key-released   no-fn
   :key-typed      no-fn
   :mouse-pressed  (fn [& args] (println args))
   :mouse-released no-fn
   :mouse-moved    no-fn
   :mouse-dragged  no-fn
   :mouse-entered  no-fn
   :mouse-exited   no-fn
   :mouse-clicked  no-fn
   :focus-gained   no-fn
   :focus-lost     no-fn})

(defn setup [a b])
(defn teardown [this])

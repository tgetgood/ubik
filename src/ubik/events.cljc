(ns ubik.events
  (:require [ubik.geometry :as geo]
            [ubik.db :as db]))

(def events
  "List of known event types."
  [::mouse-down
   ::mouse-up
   ::mouse-move
   ::touch-start
   ::touch-move
   ::touch-end
   ::wheel
   ::key-down
   ::key-up])

(def event-queue (atom #?(:clj clojure.lang.PersistentQueue/EMPTY
                          :cljs (.-EMPTY PersistentQueue))))

(def handlers (atom {}))

(defn add-handlers [hs]
  (let [hs (into {} (map (fn [[k v]] [k (if (sequential? v) v [v])]) hs))]
    (swap! handlers #(merge-with concat % hs))))

(defmulti run-effect (fn [[k v]] k))

(defmethod run-effect :swap!
  [[_ v]]
  (apply swap! @db/app-db v))

(defmethod run-effect :reset!
  [[_ v]]
  (reset! @db/app-db v))

(defn effects [m]
  (assert (not (and (:swap! m) (:reset! m))))
  (run! run-effect m))

(defn handle-event [e handlers]
  (doseq [handler (get handlers (:type e))]
    (effects
     (handler e @db/app-db))))

(let [reading? (atom false)]
  (add-watch event-queue ::stream
             (fn [_ r _ q]
               (when (compare-and-set! reading? false true)
                 (when-let [e (peek q)]
                   (handle-event e @handlers)
                   (swap! r pop)
                   (reset! reading? false))))))

(defn enqueue [event]
  (swap! event-queue conj event))

(defn dispatcher
  "Returns an event dispatch fn."
  ;; TODO: Will eventually need to use a queue and not block the main thread too
  ;; long. I can probably just lift the queue out of reframe
  [message-map]
  (fn dispatch! [state world event]
    (when-let [handlers (get message-map (:type event))]
      (doseq [handler handlers]
        (let [outcome (handler event state)]
          (when-let [next-event (:dispatch outcome)]
            (dispatch! state world next-event))
          (when-let [mutation (:mutation outcome)]
            (db/handle-mutation mutation)))))))

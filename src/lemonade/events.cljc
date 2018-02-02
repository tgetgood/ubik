(ns lemonade.events
  (:require [lemonade.geometry :as geo]))

(defprotocol IEventSystem
  (setup [this dispatch-fn]
    "Initialise the event system. System should call dispatch-fn with events
    received.")
  (teardown [this]
    "Clean up and shut down this event system."))

(defn- find-keyed-in-branch [branch]
  (when (seq branch)
    (when-let [hit (drop-while #(not (contains? (meta %) :events)) branch)]
      (conj (find-keyed-in-branch (rest hit)) (geo/retree [hit])))))

(defn find-all-keyed [branches]
  (->> branches
       (mapcat find-keyed-in-branch)
       (map first)
       (remove nil?)))

(defn dispatcher
  "Returns an event dispatch fn."
  ;; TODO: Will eventually need to use a queue and not block the main thread too
  ;; long. I can probably just lift the queue out of reframe
  [state-ref world-key message-map event-middleware]
  (fn dispatch! [event]
    (when event-middleware
      (event-middleware event dispatch!))
    (when-let [handlers (get message-map (:type event))]
      (let [hits (geo/effected-branches (:location event) (get @state-ref world-key))]
        (->> hits
             find-all-keyed
             #_(#(do (println (map meta %)) %))
             (filter #(contains? handlers (-> % meta :events :key)))
             (map #((->> % meta :events :key (get handlers)) event @state-ref %))
             (map (fn [res]
                    (when-let [mutation (:mutation res)]
                      (apply swap! state-ref mutation))))
             doall)))))

(ns lemonade.events
  (:require [lemonade.geometry :as geo]
            [lemonade.state :as state]))

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

(defn invert-index
  "Takes an indexed tree of maps from A->B->C and returns a nested index from
  B->A->C"
  [m]
  (reduce (fn [acc [vo-type sub-map]]
            (reduce (fn [acc [ev-type handler]]
                      (if (contains? acc ev-type)
                        (update-in acc [ev-type vo-type] conj handler)
                        (assoc acc ev-type {vo-type (list handler)})))
                    acc sub-map))
          {} m))

(defn dispatcher
  "Returns an event dispatch fn."
  ;; TODO: Will eventually need to use a queue and not block the main thread too
  ;; long. I can probably just lift the queue out of reframe
  [message-map]
  (let [message-map (invert-index message-map)]
    (fn dispatch! [state world event]
      (when-let [handlers (get message-map (:type event))]
        (let [vos (->> (geo/effected-branches (:location event) world)
                       find-all-keyed
                       (filter #(contains? handlers (-> % meta :events :key))))]
          (doseq [vo vos]
            (let [handlers (->> vo meta :events :key (get handlers))]
              (doseq [handler handlers]
                (let [outcome (handler event state vo)]
                  (when-let [next-event (:dispatch outcome)]
                    (dispatch! state vo next-event))
                  (when-let [mutation (:mutation outcome)]
                    (state/handle-mutation mutation)))))))))))

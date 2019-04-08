(ns ubik.topology
  (:require [clojure.pprint :refer [pprint]]
            [ubik.codebase :as codebase]
            [ubik.core :as core]
            [ubik.process :as process]
            [ubik.util :as util]))

(defn set-topology! [t])
(defn current-topology [])

(def topology-signal (process/signal ::topology))

(def initial-topology
  {:nodes {::image        codebase/image-signal
           ::topology     topology-signal
           ::screen       (process/effector ::screen core/render)
           ::code-display (process/effector ::screen core/display-code)

           ::core/interactions core/interactions
           ::core/edits        core/code-edits}
   :wires {}})

(def topology
  "Maps node names to actual runtime objects."
  (atom initial-topology))

(add-watch topology ::signal-watcher
           (fn [_ _ _ s]
             (process/send topology-signal s)))

(defn destroy! []
  (reset! topology initial-topology))

(defn init-topology! [{:keys [nodes wires]}]
  (doseq [node nodes]
    (let [name (:name node)]
      (if (contains? (:nodes @topology) name)
        (util/log :warn "Trying to readd" name "to node graph.")
        (swap! topology update :nodes assoc name node))))
  (util/log :debug {:node-list (keys (:nodes @topology))})
  (let [nodes (:nodes @topology)]
    (doseq [[out in] wires]
      (let [in-node (get nodes in)
            ;; Another bit of programmer convenience sprinkled haphazardly
            ;; about.
            out (if (map? out) out {:in out})]
        (doseq [[wire sig] out]
          (let [out-node (get nodes sig)]
            (util/log :debug "Connecting:" sig wire in)
            (process/wire in-node wire out-node)
            (swap! topology assoc-in [:wires in wire] out)))))))

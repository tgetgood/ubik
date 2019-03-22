(ns ubik.topology
  (:require [taoensso.timbre :as log]
            [ubik.process :as process]))

(defn set-topology! [t])
(defn current-topology [])

(defonce
  ^{:doc     "Maps node names to actual runtime objects."
    :private true}
  node-map
  (atom {::image (process/signal ::image)}))

(defonce
  ^{:doc     "Maps each node to the set of nodes to which it listens."
    :private true}
  connectome
  (atom {}))

(defn init-topology! [{:keys [nodes wires]}]
  ;; Don't allow nodes to be clobbered.
  ;; TODO: error messages.
  (doseq [node nodes]
    (let [name (or (:name (meta node)) (:name node))]
      (if (contains? @node-map name)
        (log/warn "Trying to readd" name "to node graph.")
        (swap! node-map assoc name node))))
  (doseq [[out in] wires]
    (let [in-node (get @node-map in)]
      (doseq [[wire sig] out]
        (let [out-node (get @node-map sig)]
          (process/wire in-node wire out-node)
          (swap! connectome update in assoc wire out))))))

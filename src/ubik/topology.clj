(ns ubik.topology
  (:require [ubik.process :as process]))

(defn set-topology! [t])
(defn current-topology [])

(defonce node-map
  (atom {::image (process/signal ::image)}))

(defn init-topology! [k {:keys [sinks sources nodes wires] :as t}]
  )

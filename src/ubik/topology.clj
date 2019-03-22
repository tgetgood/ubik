(ns ubik.topology)

(defn set-topology! [t])
(defn current-topology [])

(def image-signal)
(defonce node-map
  (atom {::image image-signal}))

(defn init-topology! [k {:keys [sinks sources nodes wires] :as t}]
  )

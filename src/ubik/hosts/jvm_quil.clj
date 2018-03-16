(ns ubik.hosts.jvm-quil
  (:require [ubik.core :as core]
            [ubik.renderers.quil :as renderer]
            [quil.core :as q]))

(defn host [{:keys [size]}]
  (let [applet (q/sketch :size size :renderer :p2d)
        g      (.-g applet)]
    {:width     (.width g)
     :height    (.height g)
     :g         g
     :render-fn (renderer/renderer g)}))

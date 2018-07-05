(ns ubik.hosts.jvm-quil
  (:require [ubik.renderers.quil :as renderer]
            [quil.core :as q]))

(defonce quil-errors (atom []))

(defn host [{:keys [size] :or {size [500 500]}}]
  (let [go?                 (ref false)
        f                   (ref (constantly nil))
        ^quil.Applet applet (q/sketch :size size :renderer :java2d
                                      :features #{:resizable
                                                  :no-bind-output}
                                      :draw (fn [] (@f))
                                      :setup (fn []
                                               (q/pixel-density
                                                (q/display-density))))
        g                   (.-g applet)]
    {:width     (fn [] (.width g))
     :height    (fn [] (.height g))
     :applet    applet
     :render-fn (fn [shape]
                  (dosync
                   (ref-set go? true)
                   (ref-set f
                            (fn []
                              (dosync
                               (when @go?
                                 (ref-set go? false)
                                 (try
                                   (renderer/renderer applet shape)
                                   (catch Exception e
                                     (.exit applet)
                                     (swap! quil-errors conj e)))))))))}))

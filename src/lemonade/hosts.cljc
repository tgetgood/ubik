(ns lemonade.hosts
  #?(:cljs
     (:require [goog.object :as obj]
               [lemonade.events.canvas :as events]
               [lemonade.renderers.canvas :as canvas-renderer]
               [lemonade.coordinates :as coords])))

#?(:cljs
   (defn html-canvas [element]
     "Runs lemonade in a the given <canvas> element in the browser."
     {:event-system (events/event-system element)
      :width        (fn [] (obj/get element "width"))
      :height       (fn [] (obj/get element "height"))
      :interceptor  #(coords/wrap-invert-coordinates % element)
      :render-fn    (partial canvas-renderer/draw! element)}))

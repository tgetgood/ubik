(ns lemonade.hosts
  #?(:cljs
     (:require [lemonade.events.canvas :as events]
               [lemonade.renderers.canvas :as canvas-renderer])))

#?(:cljs
   (defn html-canvas [element]
     "Runs lemonade in a the given <canvas> element in the browser."
     {:event-system (events/event-system element)
      :render-fn (partial canvas-renderer/draw! element)}))

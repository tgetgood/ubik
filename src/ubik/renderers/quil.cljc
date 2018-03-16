(ns ubik.renderers.quil
  (:require [ubik.core :as core]
            [ubik.renderers.util :as util]
            [quil.core :as q]))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [graphics]
  (fn [shape]
    (q/with-graphics graphics
      (q/clear)
      (q/background 255)
      (q/line 0 0 400 400)
      )
    #_(render-fn {:zoom 1 :style {}} shape)))

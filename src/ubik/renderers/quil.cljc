(ns ubik.renderers.quil
  (:require [ubik.core :as core]
            [ubik.renderers.util :as util]
            [quil.core :as q]))

(defprotocol QuilRenderable
  (compile* [this]))

(def t (atom nil))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [graphics shape]
  #_(q/with-graphics graphics
    (doto graphics
      (.clear )
      (.background 255)
      (.stroke 0 255 129)
      (.line 0 0 400 400)
      (.stroke 0)
      (.line 500 0 0 500)

      (.stroke 0 0 0 0)
      (.fill 255 128 14)
      (.ellipse 300 300 150 70)))
  (q/clear)
  (q/fill 0 0 0 0)
  (q/stroke 0)
  (q/background 255)

  (q/line 0 0 400 400)

  (q/fill 128 240 13)
  (q/stroke 0 0 0 0)
  (q/ellipse 300 300 150 70)

  (q/stroke 128)
  (q/line 300 500 100 200)

  (let [s (.createShape graphics)]
    (doto s
      (.beginShape)
      (.vertex 20 20)
      (.stroke 0 0 0 0)
      (.vertex 20 80)
      (.vertex 80 170)
      (.stroke 255 0 0)
      (.fill 255 255 123)
      (.vertex 10 110)
      (.vertex 20 20)
      (.endShape))
    (q/shape s)
    (q/scale 2)
    (q/rotate -0.54)
    (q/shape s)
    )

  #_(render-fn {:zoom 1 :style {}} shape))

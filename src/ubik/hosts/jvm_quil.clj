(ns ubik.hosts.jvm-quil
  (:require [ubik.core :as core]
            [ubik.renderers.quil :as renderer]
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
                  ;; FIXME: This is not the right place to respond to
                  ;; resizing. The draw method needs to be reinvoked on resize.
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

(def test-image
  (-> core/line
      (core/scale 100)
      (core/rotate 45)
      (core/translate [300 300])))

(alias 'l 'ubik.core)

(def ex
  [(-> l/polyline
       (assoc :points [[0 0] [100 100] [300 100] [100 300] [0 0]]
              :style {:stroke :red
                      :fill   :purple})
       (l/tag ::poly)
       (l/scale 3)
       (l/rotate 20)
       (l/translate [300 40])
       (l/tag ::translate))
   (assoc l/line :from [800 100] :to [900 100])
   (assoc l/arc :centre [0 0] :radius 200 :style {:stroke :red} :from 0 :to 1)
   (l/with-style {:fill :pink}
     (-> l/annulus
         (assoc :outer-radius 300
                :inner-radius 200
                :style {:fill   :red
                        :stroke :blue})
         (l/translate [500 500])))

   (l/scale l/circle [4000 500])])

(require '[clojure.pprint :refer [pp pprint]])

(defonce the-host (atom (host {})))

(defn re []
  (when @the-host
    (.exit ^quil.Applet (:applet @the-host)))
  (reset! the-host (host {}))
  (core/draw! ex @the-host))

(when @the-host
  (core/draw! ex @the-host))

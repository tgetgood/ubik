(ns ubik.hosts.jvm-quil
  (:require [ubik.core :as core]
            [ubik.renderers.quil :as renderer]
            [quil.core :as q]))

(defonce error-log (atom []))

(defn host [{:keys [size] :or {size [500 500]}}]
  (let [f      (atom (constantly nil))
        applet (q/sketch :size size :renderer :p2d
                         :resizable true
                         :draw (fn [] (@f)))
        g      (.-g applet)]
    {:width     (fn [] (.width g))
     :height    (fn [] (.height g))
     :applet    applet
     :render-fn (fn [shape]
                  (reset! f (fn []
                              (try
                                (renderer/renderer g shape)
                                (catch Exception e
                                  (.exit applet)
                                  (swap! error-log conj e))))))}))

(def test-image
  (-> core/line
      (assoc :to [100 10])

      ;; (core/scale 300)
      #_(core/rotate 45)
      ))

(alias 'l 'ubik.core)


(def ex
  [(-> l/polyline
       (assoc :points [[0 0] [100 100] [300 100] [100 300] [0 0]]
              :style {:stroke :cyan
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

(defonce the-host (atom nil))

(when @the-host
  (.exit (:applet @the-host)))
(reset! the-host (host {}))
(core/draw! test-image @the-host)

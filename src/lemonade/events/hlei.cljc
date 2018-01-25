(ns lemonade.events.hlei
  (:require [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.math :as math]))

(defn now []
  #?(:cljs (js/Date.now)
     :clj (System/currentTimeMillis)))

;; Click conditions
(def click-timeout 200)        ; ms
(def click-move-threshold 100) ; px^2

(def handlers
  (let [drag-state (atom nil)
        down       (atom nil)
        drag-start (atom nil)]
    #:lemonade.events
    {:wheel           (fn [ev]
                        {:dispatch (assoc ev :type ::events/scroll)
                         :stop      true})

     :left-mouse-down (fn [{:keys [location]}]
                        (reset! drag-state location)
                        (reset! drag-start location)
                        (reset! down (now))
                        {:stop true})

     :mouse-move      (fn [{:keys [location]}]
                        (when @drag-state
                          (let [delta (mapv - @drag-state location)]
                            (reset! drag-state location)
                            {:dispatch {:type  ::events/left-drag
                                        :delta delta}
                             :stop      true})))

     :left-mouse-up   (fn [{:keys [location] :as ev}]
                        (let [d     @down
                              start @drag-start]
                          (reset! drag-state nil)
                          (reset! drag-start nil)
                          {:stop true
                           :dispatch
                           (when (and
                                  (< (- (now) d) click-timeout)
                                  (< (math/norm (map - start location))
                                     click-move-threshold))
                             (assoc ev :type ::events/left-click))}))}))

(defn wrap [render]
  (fn [state]
    (let [w (render state)]
      (with-meta
        (assoc
         (core/composite {} [w])
         ::events/handlers handlers)
        (meta w)))))

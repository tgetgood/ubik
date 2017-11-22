(ns lemonade.events.hlei
  (:require [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.geometry :as geometry]))

(defn now []
  #?(:cljs (js/Date.now)
     :clj (System/currentTimeMillis)))

(def click-timeout 200)        ; ms
(def click-move-threshold 100) ; px^2

(def handlers
  (let [drag-state (atom nil)
        down (atom nil)
        drag-start (atom nil)]
    #::events
    {:wheel           (fn [_ ev]
                        (events/dispatch! (assoc ev :type ::events/scroll))
                        ::events/stop)

     :left-mouse-down (fn [_ {:keys [location]}]
                        (reset! drag-state location)
                        (reset! drag-start location)
                        (reset! down (now))
                        ::events/stop)

     :mouse-move      (fn [state {:keys [location]}]
                        (when @drag-state
                          (let [delta (mapv - @drag-state location)]
                            (reset! drag-state location)
                            (events/dispatch! {:type ::events/left-drag
                                               :delta delta})
                            ::events/stop)))

     :left-mouse-up   (fn [_ {:keys [location] :as ev}]
                        (reset! drag-state nil)
                        (when-let [d @down]
                          (when (and
                                 (< (- (now) d) click-timeout)
                                 (< (geometry/norm (map - @drag-start location))
                                    click-move-threshold))
                            (events/dispatch!
                             (assoc ev  :type ::events/left-click))))
                        (reset! drag-start nil)
                        ::events/stop)}))

(defn wrap [render]
  (fn [state]
    (assoc
     (core/composite {} [(render state)])
     ::events/handlers handlers)))

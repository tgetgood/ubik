(ns ubik.hlei
  (:require [ubik.core :as core]
            [ubik.math :as math]))

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
    {:wheel           (fn [ev]
                        {:dispatch (assoc ev :type :scroll)})

     :left-mouse-down (fn [{:keys [location]}]
                        (reset! drag-state location)
                        (reset! drag-start location)
                        (reset! down (now)))

     :mouse-move      (fn [{:keys [location]}]
                        (if @drag-state
                          (let [delta (mapv - @drag-state location)]
                            (reset! drag-state location)
                            {:dispatch {:type  :left-drag
                                        :delta (update delta 1 -)}})
                          {:dispatch {:type :hover
                                      :location location}}))

     :left-mouse-up   (fn [{:keys [location] :as ev}]
                        (let [d     @down
                              start @drag-start]
                          (reset! drag-state nil)
                          (reset! drag-start nil)
                          {:dispatch
                           (when (and
                                  (< (- (now) d) click-timeout)
                                  (< (math/norm (map - start location))
                                     click-move-threshold))
                             (assoc ev :type :left-click))}))}))

(defn expand [event dispatch-fn]
  (when-let [handler (get handlers (:type event))]
    (when-let [sub-events (:dispatch (handler event))]
      (if (sequential? sub-events)
        (doall (map dispatch-fn sub-events))
        (dispatch-fn sub-events)))))

(ns ubik.interactive.events.browser
  (:require [clojure.string :as string]
            ))

(defn canvas-elem []
  (js/document.getElementById "canvas"))

(defn- kw->js
  "Returns js property name corresponding to idiomatic keyword."
  [kw]
  (string/replace (name kw) #"-" ""))

(defn- oget
  "Unsafe and fast version of goog.object/get."
  [o k]
  (unchecked-get o k))

(defn- pixel-point
  "Returns pixel clicked on relative to canvas element. Assumes origin in the
  bottom left."
  [elem e]
  [(- (oget e "clientX") (oget elem "offsetLeft"))
   (- (oget elem "height")
      (- (oget e "clientY") (oget elem "offsetTop")))])

(defn- now []
  (js/Date.now))

(defn ^:private event-map
  "Returns a map of event handlers for elem."
  [elem dispatch-fn]
  {:context-menu (fn [e]
                   (.preventDefault e)
                   (.stopPropagation e))

   ;; This is to make sure keyboard events happen at all.
   :mouse-over   (fn [e]
                   (.preventDefault e)
                   (.focus elem))

   :mouse-down   (fn [e]
                   (.preventDefault e)
                   (let [b (.-button e)
                         p (pixel-point elem e)]
                     (case b
                       ;; Only handling left click for now.
                       0 (dispatch-fn {:type     :left-mouse-down
                                       :location p
                                       :time     (now)})
                       nil)))

   :touch-move   (fn [e]
                   (.preventDefault e))

   :mouse-move   (fn [e]
                   (.preventDefault e)
                   (dispatch-fn {:type     :mouse-move
                                 :location (pixel-point elem e)
                                 :time     (now)}))

   :mouse-up     (fn [e]
                   (.preventDefault e)
                   ;; REVIEW: Is this really to right place to decide what
                   ;; buttons a mouse has? We do need some kind of layer between
                   ;; "button 0" and "left click", but here might not be the
                   ;; place...
                   (let [b (.-button e)
                         p (pixel-point elem e)]
                     (case b
                       ;; Only handling left click for now.
                       0 (dispatch-fn {:type     :left-mouse-up
                                       :location p
                                       :time     (now)})
                       nil)))

   :wheel        (fn [e]
                   (.preventDefault e)
                   (let [mag (if (= 1 (oget e "deltaMode")) 15 1)
                         dx  (* mag (js/parseInt (oget e "deltaX")))
                         dy  (* mag (js/parseInt (oget e "deltaY")))]
                     (dispatch-fn {:type     :wheel
                                   :location (pixel-point elem e)
                                   :time     (now)
                                   :dx       dx
                                   :dy       dy})))

   :key-down     (fn [e]
                   (.preventDefault e)
                   (dispatch-fn {:type :key-down
                                 :time (now)
                                 :key (.-key e)
                                 :key-code (.-keyCode e)}))

   :key-up       (fn [e]
                   (.preventDefault e)
                   (dispatch-fn {:type :key-up
                                 :time (now)
                                 :key (.-key e)
                                 :key-code (.-keyCode e)}))})

(defonce ^:private registered-listeners (atom {}))

(defn teardown [elem]
  (doseq [[event cb] @registered-listeners]
    (.removeEventListener elem (kw->js event) cb)))

(defn setup [elem dispatch-fn]
  (when (seq @registered-listeners)
    (teardown elem))
  (let [handlers (event-map elem dispatch-fn)]
    (reset! registered-listeners handlers)
    (doseq [[event cb] handlers]
      (.addEventListener elem (kw->js event) cb))))

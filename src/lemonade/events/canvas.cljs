(ns lemonade.events.canvas
  (:require [clojure.string :as string]
            [lemonade.events :as events]))

(defn- kw->js [kw]
  (string/replace (name kw) #"-" ""))

(defn pixel-point
  "Returns pixel clicked on relative to canvas element."
  [elem e]
  [(- (.-clientX e) (.-offsetLeft elem)) (- (.-clientY e) (.-offsetTop elem))])

(defn ^:private event-map
  [elem]
  {:context-menu (fn [e]
                   (.preventDefault e)
                   (.stopPropagation e))
   ;; We need this to get the focus back to the element for keystrokes.
   ;; REVIEW: Is this the right level for this?
   :click        (fn [e]
                   (.preventDefault e)
                   (.focus elem))

   :mouse-down   (fn [e]
                   (.preventDefault e)
                   (let [b (.-button e)
                         p (pixel-point elem e)]
                     (case b
                       ;; Only handling left click for now.
                       0 (events/dispatch! {:type     ::events/left-mouse-down
                                            :location p})
                       nil)))

   :mouse-move   (fn [e]
                   (.preventDefault e)
                   (events/dispatch! {:type     ::events/mouse-move
                                      :location (pixel-point elem e)}))

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
                       0 (events/dispatch! {:type     ::events/left-mouse-up
                                            :location p})
                       nil)))

   :wheel        (fn [e]
                   (.preventDefault e)
                   (events/dispatch! {:type     ::events/wheel
                                      :location (pixel-point elem e)
                                      :dx       (js/parseInt (.-deltaX e))
                                      :dy       (js/parseInt (.-deltaY e))}))

   :key-down     (fn [e]
                   (.preventDefault e)
                   ;; TODO: Process
                   (events/dispatch! {:type ::events/key-down
                                      :raw  e}))

   :key-up       (fn [e]
                   (.preventDefault e)
                   (events/dispatch! {:type ::events/key-up
                                      :raw  e}))})

(defonce ^:private registered-listeners (atom {}))

(defn event-system [elem]
  {:teardown (fn []
               (doseq [[event cb] @registered-listeners]
                 (.removeEventListener elem (kw->js event) cb)))

   :setup    (fn []
               (let [handlers (event-map elem)]
                 (reset! registered-listeners handlers)
                 ;; Force keypresses to be allowed on canvas.
                 ;; REVIEW: This will prevent us from playing nice with others.
                 (.focus elem)
                 (doseq [[event cb] handlers]
                   (.addEventListener elem (kw->js event) cb))))})

(ns lemonade.events.canvas
  (:require [clojure.string :as string]))

(defn- kw->js [kw]
  (string/replace (name kw) #"-" ""))

(defn pixel-point
  "Returns pixel clicked on relative to canvas element."
  [elem e]
  [(- (.-clientX e) (.-offsetLeft elem)) (- (.-clientY e) (.-offsetTop elem))])

(defn ^:private event-map
  [elem fire!]
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
                       0 (fire! {:type     :lemonade.events/left-mouse-down
                                 :location p})
                       nil)))

   :mouse-move   (fn [e]
                   (.preventDefault e)
                   (fire! {:type     :lemonade.events/mouse-move
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
                       0 (fire! {:type     :lemonade.events/left-mouse-up
                                 :location p})
                       nil)))

   :wheel        (fn [e]
                   (.preventDefault e)
                   (fire! {:type     :lemonade.events/wheel
                           :location (pixel-point elem e)
                           :dx       (js/parseInt (.-deltaX e))
                           :dy       (js/parseInt (.-deltaY e))}))

   :key-down     (fn [e]
                   (.preventDefault e)
                   ;; TODO: Process
                   (fire! {:type :key-down
                           :raw  e}))

   :key-up       (fn [e]
                   (.preventDefault e)
                   (fire! {:type :key-up
                           :raw  e}))})

(defonce ^:private registered-listeners (atom {}))

(defn event-system [elem]
  {:teardown (fn []
               (doseq [[event cb] @registered-listeners]
                 (.removeEventListener elem (kw->js event) cb)))

   :setup (fn [fire!]
            (let [handlers (event-map elem fire!)]
              (reset! registered-listeners handlers)
              ;; HACK: Allows keypress events on canvas
              (.focus elem)
              (doseq [[event cb] handlers]
                (.addEventListener elem (kw->js event) cb))))})

(defn init-event-system! [elem]
  (let [es (event-system elem)]
    ((:teardown es))
    ((:setup es))))

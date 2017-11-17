(ns lemonade.events
  (:require [clojure.string :as string]))

(defonce ^:private registered-callbacks (atom {}))

(defn- kw->js [kw]
  (string/replace (name kw) #"-" ""))

(defn pixel-point
  "Returns pixel clicked on relative to canvas element."
  [elem e]
  [(- (.-clientX e) (.-offsetLeft elem)) (- (.-clientY e) (.-offsetTop elem))])

(defn fire! [evt]
  #_(println evt))

#?(:cljs
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
                          0 (fire! {::type     ::left-mouse-down
                                    ::location p})
                          nil)))
      :mouse-move   (fn [e]
                      (.preventDefault e)
                      (fire! {::type     ::mouse-move
                              ::location (pixel-point elem e)}))

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
                          0 (fire! {::type     ::left-mouse-up
                                    ::location p})
                          nil)))
      :wheel        (fn [e]
                      (.preventDefault e)
                      (fire! {::type     ::wheel
                              ::location (pixel-point elem e)
                              ::dx       (js/parseInt (.-deltaX e))
                              ::dy       (js/parseInt (.-deltaY e))}))

      :key-down     (fn [e]
                      ;; TODO: Process
                      (fire! {::type ::key-down
                              ::raw  e}))

      :key-up       (fn [e]

                      (.preventDefault e)
                      (fire! {::type ::key-up
                              ::raw  e}))}))

#?(:cljs
   (defn init-event-system! [elem]

     ;; Stop previous handlers
     (doseq [[event cb] @registered-callbacks]
       (.removeEventListener elem (kw->js event) cb))

     ;; Setup new ones
     (let [handlers (event-map elem)]
       (reset! registered-callbacks handlers)
       ;; HACK: Allows keypress events on canvas
       (.focus elem)
       (doseq [[event cb] handlers]
         (.addEventListener elem (kw->js event) cb)))))

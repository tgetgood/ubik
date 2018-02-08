(ns lemonade.renderers.canvas
  "HTML Canvas renderer. Technically an ad hoc compiler."
  (:require [goog.object :as obj]
            [lemonade.core :as core]
            [lemonade.renderers.util :as util]))

(def default-render-state {:style {} :zoom 1 :in-path? false})

(defn join [lists]
  (apply concat (remove empty? lists)))

(defn safely [& lists]
  (concat [["save"]]
          (join lists)
          [["restore"]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti style-prop
  "Returns code for setting a single style property."
  (fn [state [k v]] k))

(defn style-wrapper
  "Returns a minimal sequence of instructions for transitioning from the current
  global style (stored in state) to the desired style."
  [state style]
  (let [parent-style (:style state)
        child-style (merge {:stroke :black} style)]
    (->> child-style
         (filter (fn [[k v]] (or (not (contains? parent-style k))
                                (= (get parent-style k) v))))
         (mapcat #(style-prop state %)))))

(def prop-meta {:setter true})

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  "rgba(0,0,0,0)"
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        "rgba(0,0,0,0)"))

(defmethod style-prop :stroke
  [{:keys [zoom]} [_ v]]
  (let [c (process-colour v)]
    (when c
      (mapv #(with-meta % prop-meta)
            [["lineWidth" (/ 1 zoom)]
             ["strokeStyle" c]]))))

(defmethod style-prop :fill
  [_ [_ v]]
  (let [c (process-colour v)]
    (when c
      [(with-meta ["fillStyle" c] prop-meta)])))

(defmethod style-prop :opacity
  [_ [_ v]]
  [(with-meta ["globalAlpha" v] prop-meta)])

(defmethod style-prop :font
  [_ [_ v]]
  [(with-meta ["font" v] prop-meta)])

(defn with-style [state style & cmds]
  (let [styles (style-wrapper state style)]
    (if (empty? styles)
      (join cmds)
      (apply safely styles cmds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-wrapper [state style [x y] & lists]
  (let [in? (:in-path? state)]
    (with-style state style
      (when-not in?
        [["beginPath"]])
      (when (or (:override state) (not in?))
        [["moveTo" x y]])
      (join lists)
      (when-not in?
        [["stroke"]]))))

(defprotocol Canvas2DRenderable
  (compile-renderer [this state]
    "Returns instructions for rendering this a ctx."))

(extend-protocol Canvas2DRenderable
  default
  (compile-renderer [this state]
    (if (core/template? this)
      (compile-renderer (core/expand-template this) state)
      (println (str "I don't know how to render a " this ". Aborting."))))

  nil
  (compile-renderer [_ _]
    (println "Cannot render nil. Aborting.'"))

  core/AffineTransformation
  (compile-renderer [{{[a b c d] :matrix [e f] :translation} :atx
                      base                                   :base-shape}
                     state]
    (safely [["transform" a b c d e f]]
            (compile-renderer base (update state :zoom *
                                           (util/magnitude a b c d)))))

  PersistentVector
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  List
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  LazySeq
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  core/Composite
  (compile-renderer [{:keys [style contents]} state]
    (with-style state style
      (mapcat compile-renderer
              contents
              (repeat (update state :style #(merge style %))))))

  core/Frame
  (compile-renderer [{base :base-shape w :width h :height [x y] :corner} state]
    (safely [["beginPath"]
             ["rect" x y w h]
             ["clip"]]
            (compile-renderer base state)))

  core/Region
  (compile-renderer [{:keys [style boundary]} state]
    (let [substate  (-> state
                        (assoc :in-path? true)
                        (update :style #(merge style %)))
          substates (cons (assoc substate :override true)
                          (repeat substate))]
      (with-style state style
        [["beginPath"]]
        (mapcat compile-renderer boundary substates)
        [["closePath"]]
        (when-let [fill (-> substate :style :fill)]
          (when (not= fill :none)
            [["fill"]]))
        [["stroke"]])))

  core/Line
  (compile-renderer [{[x y] :to :keys [from style]} state]
    (path-wrapper state style from
      [["lineTo" x y]]))

  core/Arc
  (compile-renderer [arc state]
    (let [{r :radius [x y] :centre :keys [from to style clockwise?]} arc]
      (path-wrapper
        (assoc state :override (:jump (meta arc)))
        style
        [(+ x r) y]
        [["arc" x y r from to (boolean clockwise?)]])))

  core/Bezier
  (compile-renderer [{[x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2 :keys [style from]}
                     state]
    (path-wrapper state style from
                  [["bezierCurveTo" cx1 cy1 cx2 cy2 x2 y2]]))

  core/RawText
  (compile-renderer [{[x y] :corner :keys [style text]} state]
    (with-style state style
      [["fillText" text x y]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- execute!
  "Given a sequence of rendering operations and a context, carry them out"
  [ctx cmds]
  (loop [[cmd & rest] cmds]
    (when cmd
      (if (-> cmd meta :setter)
        (apply obj/set ctx cmd)
        (apply js-invoke ctx cmd))
      (recur rest))))

(defn- clear-screen!
  "Destructively wipes the screen."
  [ctx w h]
  (.clearRect ctx 0 0 w h))

(defn- context [elem]
  (.getContext elem "2d"))

(defn draw!
  "Draw world to HTML Canvas element."
  [canvas-element world]
  (let [cmds (compile-renderer world default-render-state)]
    (doto (context canvas-element)
      (clear-screen! (.-width canvas-element) (.-height canvas-element))
      (execute! cmds))))

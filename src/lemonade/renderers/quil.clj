(ns lemonade.renderers.quil
  (:require [lemonade.core :as core]
            [lemonade.renderers.util :as util]
            [quil.core :as q]))

(defmulti render-fn (fn [state shape] (core/classify shape)))

(defn render-all [states shapes]
  (mapcat render-fn states shapes))

(defmethod render-fn ::core/sequential
  [state shape]
  (render-all (repeat state) shape))

(defmethod render-fn ::core/template
  [state shape]
  (render-fn state (core/template-expand shape)))

(defmethod render-fn :default
  [state shape]
  (util/render-catchall shape))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [shape]
  (concat
   [q/clear
    [q/background 255]
    q/reset-matrix]
   (render-fn {:zoom 1 :style {}} shape)))

(defn render
  "Experimental renderer. Renderer returns a list of instructions (and is very
  cachable) and render just iterates through and execute them."
  [shape]
  (loop [[i & more] (renderer shape)]
    (when i
      (cond
        (fn? i)         (i)
        (fn? (first i)) (apply (first i) (rest i))
        :else (throw (Exception. (str "Bad instruction: " i))))
      (recur more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/atx
  [state {:keys [base-shape] {[a b c d] :matrix [e f] :translation} :atx}]
  (let [mag (util/magnitude a b c d)]
    (concat
     [q/push-matrix
      [q/apply-matrix a b e c d f]]
     (render-fn (update state :zoom * mag) base-shape)
     [q/pop-matrix])))

(defmethod render-fn ::core/path
  [state {:keys [closed? contents style]}]
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/line
  [state {:keys [from to]}]
  [[q/line from to]])

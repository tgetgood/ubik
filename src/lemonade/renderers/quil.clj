(ns lemonade.renderers.quil
  (:require [lemonade.core :as core]
            [lemonade.renderers.util :as util]
            [quil.core :as q]))

(defmulti render-fn (fn [state shape] (:type shape)))

(defmethod render-fn :default
  [state shape]
  ;; TODO: Refactor to use core/classify
  (cond
    (sequential? shape)
    (mapcat (partial render-fn state) shape)

    (contains? (methods core/template-expand) (:type shape))
      (render-fn state (core/template-expand shape))

    :else
      (do
        (println (str "I don't know how to render a "
                      (or (:type shape) (type shape))))
        util/noop)))

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
    (if (fn? i)
      (i)
      (apply (first i) (rest i)))
    (when more
      (recur more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/atx
  [state {:keys [base-shape] {[a b c d] :matrix [e f] :translation} :atx}]
  (concat
   [q/push-matrix
    [q/apply-matrix a b e c d f]]
   ;; TODO: stroke width inversion
   (render-fn state base-shape)
   [q/pop-matrix]))

(defmethod render-fn ::core/path
  [state {:keys [closed? contents style]}]
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/line
  [state {:keys [from to]}]
  [[q/line from to]])

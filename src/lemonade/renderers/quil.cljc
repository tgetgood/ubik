(ns lemonade.renderers.quil
  (:require [lemonade.core :as core]
            [quil.core :as q]))

(def noop (constantly nil))

(defmulti render-fn :type)

(defmethod render-fn :default
  [x]
  ;; FIXME: Copy paste
  (cond
    (sequential? x)
      (apply juxt (map render-fn x))

    (contains? (set (keys (methods core/template-expand))) (:type x))
      (render-fn (core/template-expand x))

    :else
      (do
        (println (str "I don't know how to render a " (:type x)))
        noop)))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [shape]
  (render-fn shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/atx
  [{:keys [base-shape atx]}]
  (let [cont (render-fn base-shape)
        {[a b c d] :matrix [e f] :translation} atx]
    (fn []
      (q/push-matrix)
      #?(:cljs (q/apply-matrix a b 0 e c d 0 f 0 0 1 0 0 0 0 1)
         :clj  (q/apply-matrix a b e c d f))
      (cont)
      (q/pop-matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/line
  [{:keys [from to]}]
  (fn []
    (q/line from to)))

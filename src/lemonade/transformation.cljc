(ns lemonade.transformation
  "AST transformations."
  (:require [clojure.walk :as walk]
            [lemonade.core :as core]))

(defn template-expand [tree]
  ;; REVIEW: Do I need to memoise this?
  ;; This is probably a much more efficient way to expand templates than I'm
  ;; currently doing. But it lacks in dynamism.
  (walk/postwalk (memoize (fn [node]
                            (if (map? node)
                              (core/template-expand-all node)
                              node)))
                 tree))

(defn name-atx [atx]
  (let [{t :atx-type :as mm} (meta atx)]
    (cond
      (= t :translation)
      (str "Translation by " (:point mm))

      (= t :rotation)
      (str "Rotation by " (:angle mm) " around " (:point mm))

      (= t :scale)
      (str "Scale by " (:extent mm) " around " (:point mm))

      (= t :reflection)
      (str "Reflect in line " (:dir mm) "x + " (:point mm))

      (= t ::core/window)
      ["Adjustable Window Frame" {:zoom (first (:matrix (:atx atx)))
                                  :pan (:translation (:atx atx))}]

      (= t :lemonade.core/invert-coordinates)
      "Coordinate Inversion"

      :else
      {:transformation (:atx atx)})))

;; REVIEW: This should be a straight up cond, no?
;; The only switches here are on core builtins.

(defmulti clean-node #(keyword :lemonade.core (type %)))

(defmethod clean-node ::core/atx
  [atx]
  [(name-atx atx) (clean-node (:base-shape atx))])

(defmethod clean-node ::core/composite
  [shape]
  (let [style (:style shape)
        contents (:contents shape)]
    (cond
      (and (empty? style) (= 1 (count contents)))
      (clean-node (first contents))

      (and (seq style) (= 1 (count contents)))
      [:with-style style (clean-node (first contents))]

      (seq style)
      [:with-style style (mapv clean-node contents)]

      :else
      (mapv clean-node (:contents shape)))))

(defmethod clean-node ::core/frame
  [shape]
  [(:type shape) (dissoc shape :type :contents) (clean-node (:contents shape))])

(defmethod clean-node ::core/sequential
  [shapes]
  (if (= 1 (count shapes))
    (clean-node (first shapes))
    (mapv clean-node shapes)))

(defmethod clean-node :default
  [shape]
  [(:type shape) (dissoc shape :type)])

(defn friendlify-code [tree]
  (clean-node tree))

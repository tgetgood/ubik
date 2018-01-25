(ns lemonade.transformation
  "AST transformations."
  (:require [clojure.walk :as walk]
            [lemonade.core :as core]))

(defn template-expand [tree]
  ;; REVIEW: Do I need to memoise this?
  ;; This is probably a much more efficient way to expand templates than I'm
  ;; currently doing.
  (walk/postwalk (memoize (fn [node]
                            (if (map? node)
                              (core/template-expand-all node)
                              node)))
                 tree))

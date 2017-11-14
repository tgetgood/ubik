(ns lemonade.renderers.util)

(def noop
  "What a render-fn returns if it wants to do nothing."
  (constantly nil))

(def default-style
  "Default style of images in lemonade."
  {:stroke  :black
   :fill    :none
   :opacity 1
   :font    "sans serif 10px"})

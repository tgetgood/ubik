(ns lemonade.renderers.canvas)

(defmacro with-style*
  "Applies styles and manages bookkeeping so that styles are unapplied at the
  end. cont must be a function of one argument which will be passed the current
  graphical context."
  [style cont]
  `(let [delta# (apply dissoc (flatten-style ~style) *style*)
         style-setter# (apply juxt (map style-ctx delta#))]
     (fn [ctx#]
       (.save ctx#)
       ;; (style-setter# ctx#)
       (binding [*style* (merge (flatten-style ~style) *style*)]
         (~cont ctx#))
       (.restore ctx#))))

(defmacro with-style
  "Like with-style* but executes body inside implicit (doto ctx ~@body) where
  ctx is the graphical context."
  [style & body]
  `(with-style* ~style
     (fn [ctx#]
       (doto ctx#
         ~@body))))

(ns lemonade.renderers.canvas)

(defmacro with-style*
  "Applies styles and manages bookkeeping so that styles are unapplied at the
  end. cont must be a function of one argument which will be passed the current
  graphical context."
  [style cont]
  `(let [style-setter# (safe-style ~style)]
     (fn [ctx#]
       (.save ctx#)
       (style-setter# ctx#)
       (~cont ctx#)
       (.restore ctx#))))

(defmacro with-style
  "Like with-style* but executes body inside implicit (doto ctx ~@body) where
  ctx is the graphical context."
  [style & body]
  `(with-style* ~style
     (fn [ctx#]
       (doto ctx#
         ~@body))))

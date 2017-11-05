(ns lemonade.renderers.canvas)

(defmacro with-path-style [ctx state style from & body]
  `(let [in-path# (:in-path? ~state)
         [x# y#] ~from]
     (.save ~ctx)
     (when-not in-path#
       (.beginPath ~ctx))
     (when (or (:override ~state) (not in-path#))
       (.moveTo ~ctx x# y#))
     (safe-style ~ctx ~state ~style)
     ~@body
     (when-not in-path#
       (.stroke ~ctx))
     (.restore ~ctx)))

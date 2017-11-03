(ns lemonade.renderers.canvas)

(defmacro with-path-style [state style from & body]
  `(let [stylise# (safe-style ~state ~style)
         in-path# (:in-path? ~state)
         [x# y#] ~from
         ]
     (fn [ctx#]
       (.save ctx#)
       (when-not in-path#
         (.beginPath ctx#))
       (when (or (:override ~state) (not in-path#))
         (.moveTo ctx# x# y#))
       (stylise# ctx#)
       (doto ctx#
         ~@body)
       (when-not in-path#
         (.stroke ctx#))
       (.restore ctx#))))

(ns lemonade.renderers.canvas)

(defn switch [c ctx cmd [cname nargs]]
  [(list 'identical? c cname)
   (apply list (symbol (str "." cname)) ctx
          (map (fn [i] (list 'unchecked-get cmd i)) (range 1 (inc nargs))))])

(defmacro unsafe-invoke
  {:style/indent 2
   :doc "Low level switching construct. Not cool, but rendering needs to be very
   fast."}
  [ctx cmd & paths]
  (let [c (gensym)]
    `(let [~c (~'unchecked-get ~cmd 0)]
       (cond
         ~@(mapcat (fn [path] (switch c ctx cmd path)) (partition 2 paths))
         :else (throw (js/Error. (str "Invalid canvas command: " ~cmd)))))))

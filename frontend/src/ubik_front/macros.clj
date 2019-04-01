(ns ubik-front.macros)

(defmacro record-read-handlers [& recnames]
  `(into {}
         [~@(map (fn [x]
                   [(str "falloleen.lang." x) (symbol "falloleen.lang"
                                                         (str "map->" x))])
                 recnames)]))

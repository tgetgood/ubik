(ns lemonade.macros)

(defn namespace-qualified-kw [sym]
  (if (namespace sym)
    (keyword sym)
    (let [current-ns (namespace `x)]
      (keyword current-ns (name sym)))))

(defn resolve-name [n]
  (cond
    (keyword? n) n

    (and (sequential? n)
         (= 'quote (first n))) (second n)

    :else (throw (Exception. "inapprorpriate template name"))))

(defmacro deftemplate
  "Defines a new shape template. Something like a macro"
  [template-name template expansion]
  (let [template-name (resolve-name template-name)]
    (if-not (namespace template-name)
      (throw (Exception. "Template names must be namespace qualified"))
      `(do
         (def ~(symbol (name template-name))
           ~(assoc template :type (keyword template-name)))

         (defmethod lemonade.core/template-expand ~(keyword template-name)
           [{:keys [~@(map (comp symbol name) (keys (dissoc template :type)))]}]
           ~expansion)))))

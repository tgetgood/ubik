(ns ubik.interactive.subs
  (:require [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.interactive.db :as db]
            [ubik.interactive.impl]
            [clojure.walk :as walk]))

;; REVIEW: Is this really any better than two repetitive definitions? More
;; concise but way less readable...
(deftype SimpleSubscription
    #?(:clj [dependencies reaction
             ^:volatile-mutable _last-db
             ^:volatile-mutable _last-args
             ^:volatile-mutable _last-val]
       :cljs [dependencies reaction
              ^:mutable _last-db
              ^:mutable _last-args
              ^:mutable _last-val])
  ubik.interactive.impl/Subscription
  (deps [_] dependencies)
  (debug [_] [_last-db _last-args _last-val])
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (let [app-db (db/get-current-value)]
      (if (= _last-db app-db)
        _last-val
        (let [inputs (map deref dependencies)]
          (if (= inputs _last-args)
            _last-val
            (let [next (apply reaction inputs)]
              (set! _last-db app-db)
              (set! _last-args inputs)
              (set! _last-val next)
              next)))))))

(defn subscription? [sig]
  (satisfies? ubik.interactive.impl/Subscription sig))

(defn build-subscription
  {:style/indent [1]}
  [deps reaction]
  (SimpleSubscription. deps reaction (gensym "NOMATCH") (gensym "NOMATCH") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macros/deftime

(defn sub-checker [form]
  (when (and (or (list? form) (instance? clojure.lang.Cons form))
           (= (count form) 2)
           (every? symbol? form)
           (= (resolve (first form)) #'clojure.core/deref)
           (subscription? @(resolve (second form))))
    (second form)))

(defmacro subscription
  "Given a form which derefs other subscriptions returns a new subscription that
  reacts to its dependencies. If form does not depend on any subscriptions then
  it is evaluated and it's (static) value returned."
  [form]
  (let [symbols (atom {})

        body (walk/prewalk
              (fn [f]
                (if-let [sub (sub-checker f)]
                  (if (contains? @symbols sub)
                    (get @symbols sub)
                    (let [sym (gensym)]
                      (swap! symbols assoc sub sym)
                      sym))
                  f))
              form)

        sym-seq (seq @symbols)]
    (if (empty? sym-seq)
      `(atom ~form)
      `(build-subscription  [~@(map key sym-seq)]
        (fn [~@(map val sym-seq)]
           ~body))))))

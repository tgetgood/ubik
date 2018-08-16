(ns ubik.interactive.subs
  "Subscriptions are lazy processes that run a fixed function on the current
  values of all input processes on demand and emits the result.

  Because they are lazy nothing can listen to them. They have to be actively
  derefed or nothing will ever happen."
  (:require [net.cgrand.macrovich :as macros :include-macros true]
            [clojure.walk :as walk]
            [ubik.interactive.base :as base]))

;; REVIEW: Is this really any better than two repetitive definitions? More
;; concise but way less readable...
(deftype SimpleSubscription
    #?(:clj [dependencies reaction
             ^:volatile-mutable _last-args
             ^:volatile-mutable _last-val]
       :cljs [dependencies reaction
              ^:mutable _last-args
              ^:mutable _last-val])
  base/Listener
  (inputs [_]
    (into #{} dependencies))

  base/Inspectable
  (debug [_]
    [_last-args _last-val])

  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (let [inputs (map #(if (var? %) @@% @%) dependencies)]
      (if (= inputs _last-args)
        _last-val
        (let [next (apply reaction inputs)]
          (set! _last-args inputs)
          (set! _last-val next)
          next)))))

(defn build-subscription
  {:style/indent [1]}
  [deps reaction]
  (SimpleSubscription. deps reaction (gensym "NOMATCH") nil))

(defn subscription? [x]
  (instance? SimpleSubscription x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macros/deftime

(defn sub-checker
  "Returns the second element of form iff form is the dereference of a something
  defined at the top level."
  [form]
  (when (and (or (list? form) (instance? clojure.lang.Cons form))
           (= (count form) 2)
           (every? symbol? form)
           (= (resolve (first form)) #'clojure.core/deref))
    (if  (var? (resolve (second form)))
      `(var ~(second form))
      (second form))))

(defmacro subscription
  "Given a form which derefs other refs, returns a new subscription that reacts
  to its dependencies. If form does not depend on any refs then it is evaluated
  and its (static) value returned."
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
      `(build-subscription []
         (fn []
           ~form))
      `(build-subscription  [~@(map key sym-seq)]
        (fn [~@(map val sym-seq)]

           ~body))))))

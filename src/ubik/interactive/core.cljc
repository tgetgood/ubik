(ns ubik.interactive.core
  (:require [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]))

(defn event-queue [] (atom []))

(defn start-event-processing [eq handlers effectors])

;; Token protocol
(defprotocol Signal
  (deps [_])
  (debug [_]))

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
  Signal
  (deps [_] dependencies)
  (debug [_] [_last-db _last-args _last-val])
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (let [app-db @db/app-db]
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

#?(:clj
   (defrecord RefSub [ref]
     Signal
     clojure.lang.IDeref
     (deref [_] @ref))
   :cljs
   (defrecord RefSub [ref]
     Signal
     IDeref
     (-deref [_] @ref)))

(def db (RefSub. db/app-db))

(defn subscription? [sig]
  (satisfies? Signal sig))

(defn subscription
  {:style/indent [1]}
  [deps reaction]
  (SimpleSubscription. deps reaction (gensym "NOMATCH") (gensym "NOMATCH") nil))

(macros/deftime

;; Macros

(defn intern-subscription [form table]
  (let [k  (second form)
        tv @table]
    (if (contains? tv k)
      (get tv k)
      (let [sym (gensym)]
        (if (compare-and-set! table tv (assoc tv k sym))
          sym
          (recur form table))))))

(defn sub-checker [form]
  (when (and (or (list? form) (instance? clojure.lang.Cons form))
           (= (count form) 2)
           (every? symbol? form)
           (= (resolve (first form)) #'clojure.core/deref)
           (subscription? @(resolve (second form))))
    (second form)))

(defmacro sub-form
  {:style/indent [1]
   :doc          "Returns a subscribed version of form.
  This subscription is a function which given a signal graph returns a value.
  The signal graph is just a map from keys to subscriptions.
  A subscription does not need to be part of the signal graph it receives (but
  probably will be). Recursive calls will end in disaster."}
  [form]
  (let [symbols (atom {})

        body (walk/prewalk
              (fn [f]
                (if-let [sub (sub-checker f)]
                  (let [sym (gensym)]
                    (swap! symbols assoc sub sym)
                    sym)
                  f))
              form)

        sym-seq (seq @symbols)]
    (if (empty? sym-seq)
      form
      `(subscription  [~@(map key sym-seq)]
        (fn [~@(map val sym-seq)]
           ~body)))))

(defmacro defsub [name form]
  `(def ~name ~(sub-form form)))

(defmacro defsubs [sub-map]
  `(do
     ~@(map (fn [s#] `(declare ~s#)) (keys sub-map))
     ~@(map (fn [[k# v#]] `(def ~k# (sub-form ~v#)))
           sub-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Game Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^:private continue? (atom nil))

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host check-sym]
  (let [recurrent (fn recurrent [counter last-run]
                    #?(:clj
                       ;; Need some kind of abstraction around animation frames.
                       ;; We can't be drawing in a busy loop like this
                       (core/draw! world host)
                       :cljs
                       (js/window.requestAnimationFrame
                        (fn [now]
                          (when (= check-sym @continue?)
                            (let [w @world]
                              (when-not (= @db/the-world w)
                                (core/draw! w host)
                                (reset! db/the-world w)))
                            (recurrent (inc counter) last-run))))))]
    (recurrent 0 0)))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [root host subs handlers init-db effects]}]
  (let [host (or host (hosts/default-host {}))
        eq (event-queue)]

    (when (= @db/app-db ::db/uninitialised)
      (reset! db/app-db init-db))

    (events/wire-events host eq)

    (start-event-processing eq handlers effects)
    (draw-loop root host (reset! continue? (gensym)))))

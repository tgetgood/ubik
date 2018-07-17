(ns ubik.interactive.core
  (:require [clojure.core.async :as async :include-macros true]
            [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Event Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord RHandler [in xform])
(defrecord Handler [in out xform name])

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error) m)))

(defn keyword-or-set [kos]
  (cond
    (keyword? kos) #{kos}
    (set? kos) kos
    :else (error "Handlers can only watch single events or sets of events.")))

(defn temp-key [name]
  [::temp-state name])

(defn emit
  ([db]
   (fn [rf]
     (rf db)))
  ([db ev]
   (fn [rf]
     (rf db ev)))
  ([db ev & evs]
   (fn [rf]
     (reduce rf (rf db ev) evs))))

(defn transducer
  "Create a (non-interruptable) transducer from xform, which is an fn of 2
  arguments which can all `emit` to pass values down the chain."
  [xform]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc x]
       (let [step (xform acc x)]
         (if (fn? step)
           (step rf)
           step))))))

(defn handler
  {:style/indent [1]}
  ;; TODO: This should be a macro that looks at rf and decides whether or not it
  ;; is already a transducer. If I can hide this from the user then that should
  ;; substantially improve the interface.
  ([watch f emit]
   (Handler. (keyword-or-set watch) emit f "anon$"))
  ([watch f emit name]
   (Handler. (keyword-or-set watch) emit f name)))

(defn db-handler [watch rf]
   (RHandler. (keyword-or-set watch) (transducer rf)) )

(macros/deftime

(defmacro defhandler
  {:style/indent [3]}
  [name k args methods]
  (let [multi (gensym)]
    `(do
       (defmulti ~multi (fn ~args (:type ~(second args))))
       ~@(map (fn [[k# v#]] `(defmethod ~multi ~k# ~args ~v#))
              methods)
       (def ~name
         (Handler. ~(into #{} (keys methods)) ~k (transducer ~multi) ~name))))))

(defn organise-handlers [handlers]
  (reduce (fn [hg h]
            (reduce (fn [hg w]
                      (if (contains? hg w)
                        (update hg w conj h)
                        (assoc hg w [h])))
                    hg (:in h)))
          {} handlers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Event Queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IEventQueue
  (enqueue [this v]))

(defrecord EventQueue [queue buf-count buf-size]
   IEventQueue
   (enqueue [this v]
     (swap! buf-count inc)
     (if (<= buf-size buf-count)
       (error "Too many backlogged events. Cannot recover. Aborting.")
       (async/put! queue v))
     this))

(defn set-if-nil [ms k v]
  (map (fn [m]
         (if (contains? m k)
           m
           (assoc m k v)))
       ms))

(defn shunt-rf
  ([db]
   (if (::shunted db)
     db
     {::shunted true ::db db}))
  ([db ev]
   (if (::shunted db)
     (update db ::events conj ev)
     {::shunted true ::db db ::events [ev]})))

(defn transduce-1 [xform db ev]
  (let [f (xform shunt-rf)]
    (f (f db ev))))

(defn run-queue [handlers db event]
  (let [relevant (get handlers (:type event))]
    (loop [db db
           evs []
           hs relevant]
      (if (seq hs)
        (let [h (first hs)
              res (transduce-1 (:xform h) db event)
              evs (into evs (map #(assoc % :type (:out h)) (::events res)))]
          (recur (::db res) evs (rest hs)))
        [db evs]))))

(defn create-queue [handlers]
  (let [chan (async/chan 1000)
        queue (EventQueue. chan (atom 0) 1000)]
    (async/go-loop []
      (when-let [ev (async/<! chan)]
        (try
          (swap! (:buf-count queue) dec)
          (let [db (db/get-current-value)
                [next-db evs] (run-queue handlers db ev)]
            (when (seq evs)
              (reduce enqueue queue evs))
            (db/reset-db! next-db))
          (catch #?(:clj Exception :cljs js/Error) e
            (println "We have a problem: " e)))
        (recur)))
    queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: I want undo-tree, which in this case is really undo-graph. But I need
;; to write a UI to make it remotely useful, so let's just stick to a line for
;; now
(defonce ^:private undo-graph (atom {:current nil
                                     :checkpoints {}}))

(defn checkpoint! []
  (let [db @db/app-db]
    (swap! undo-graph
           (fn [g]
             (let [current (:current g)]
               (cond-> g
                 current (update :checkpoints update current assoc :next db)
                 current (update :checkpoints assoc db {:previous current})
                 true (assoc :current db)))))))

(defn undo! []
  (let [check (:current @undo-graph)]
    (when-let [prev (-> @undo-graph :checkpoints (get check) :previous)]
      (swap! undo-graph assoc :current prev)
      (reset! db/app-db prev))))

(defn redo! []
  (let [check (:current @undo-graph)
        next (-> @undo-graph :checkpoints (get check) :next)]
    (when next
      (swap! undo-graph assoc :current next)
      (reset! db/app-db next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Distinction: subscriptions are reactive, signals are active. This is more
;; important than it may seem.
(defprotocol Subscription
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
  Subscription
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
   (deftype RefSub [ref]
     Subscription
     clojure.lang.IDeref
     (deref [_] @ref))

   :cljs
   (deftype RefSub [ref]
     Subscription
     IDeref
     (-deref [_] @ref)))

(def db (RefSub. db/app-db))

(defn subscription? [sig]
  (satisfies? Subscription sig))

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

(defmacro build-subscription
  "Given a form --- which presumably derefs other subscriptions --- return a new
  subscription that reacts to its dependencies."
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
      `(subscription  [~@(map key sym-seq)]
        (fn [~@(map val sym-seq)]
           ~body)))))

(defmacro defsub
  "Creates a subscription for form and binds it to a var with name. Sets the
  docstring approriately if provided."
  {:style/indent [1]}
  [name form]
  `(def ~name (build-subscription ~form))))

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
                       (core/draw! @world host)
                       :cljs
                       (js/window.requestAnimationFrame
                        (fn [now]
                          (when (= check-sym @continue?)
                            (let [w @world]
                              (when-not (= @db/the-world w)
                                (core/draw! w host)
                                (reset! db/the-world w)))
                            (recurrent (inc counter) now))))))]
    (recurrent 0 0)))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [root host subs handlers init-db effects]}]
  (db/set-once! init-db)
  (let [host (or host (hosts/default-host {}))
        hs (organise-handlers handlers)
        queue (create-queue hs)]
    (events/wire-events host #(enqueue queue %))

    (draw-loop root host (reset! continue? (gensym)))))

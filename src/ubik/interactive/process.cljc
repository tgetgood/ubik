(ns ubik.interactive.process
  "Processes are the fundamental unit of change in a program. Each process takes
  input from one or more processes and transduces that input into output in
  some way. It is up to a higher level of organisation to propagate the output
  signal to those processes listening for it. It is also up to a higher level of
  organisation to decide which processes are capable of listening to which (one
  standard method at present is simply to use clojure's namespaces for this
  purpose)."
  (:require [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.interactive.base :as base]))

(defprotocol Multiplexer
  "A multiplexer is a generic function which is polymorphic in the input source
  rather than the input type. "
  (method [this input])
  (add-method [this key method]))

(defprotocol EmissionTracking
  (^:private set-emission! [this val]))

(defprotocol Stateful
  (get-state [this])
  (^:private set-state! [this state]))

(def ^:dynamic *current-emitter* nil)

#?(:clj
   (defmacro emit [& args]
     `(*current-emitter* ~@args)))

;; REVIEW: do I need to implement IMeta for process types?
;; + : Most things in clojure take metadata, so it's unintuitive that these
;; don't
;; - : processes are pretty much all vars at the moment, so we have metadata
;; there. The user can't really control that though.
;; - : I don't need it at present.

(deftype StatefulProcess
    #?(:clj [method-map ^:volatile-mutable last-emission ^:volatile-mutable state]
       :cljs [method-map ^:mutable last-emission ^:mutable state])

  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    last-emission)

  Stateful
  (get-state [this]
    state)
  (set-state! [this s']
    (when-not (nil? s')
      (set! state s')))

  EmissionTracking
  (set-emission! [_ v]
    (set! last-emission v))

  Multiplexer
  (method [this input]
    (when-let [method (get method-map input)]
      (let [emitter (fn ([s]
                         (fn [rf acc]
                           (set-state! this s)
                           (rf acc)))
                      ([s ev]
                       (fn [rf acc]
                         (set-state! this s)
                         (set-emission! this ev)
                         (rf acc ev)))
                      ([s ev & evs]
                       (fn [rf acc]
                         (set-state! this s)
                         (set-emission! this (last evs))
                         (reduce rf (rf acc ev) evs))))
            trans   (fn [xform]
                      (fn [rf]
                        (fn
                          ([] (rf))
                          ([acc] (rf acc))
                          ([acc x]
                           (let [step (binding [*current-emitter* emitter]
                                        (xform x))]
                             (if (fn? step)
                               (step rf acc)
                               (do
                                 (set-state! this step)
                                 step)))))))]
        (trans
         (fn [e]
           (method state e))))))

  (add-method [_ k method]
    (StatefulProcess. (assoc method-map k method) ::uninitialised nil)))

(deftype StatelessProcess
    #?(:clj [method-map ^:volatile-mutable last-emission]
       :cljs [method-map ^:mutable last-emission])

  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    last-emission)

  EmissionTracking
  (set-emission! [_ v]
    (set! last-emission v))

  Multiplexer
  (method [this input]
    (when-let [method (get method-map input)]
      (let [emitter (fn ([]
                         (fn [rf acc]
                           (rf acc)))
                      ([ev]
                       (fn [rf acc]
                         (set-emission! this ev)
                         (rf acc ev)))
                      ([ev & evs]
                       (fn [rf acc]
                         (set-emission! this (last evs))
                         (reduce rf (rf acc ev) evs))))
            trans   (fn [xform]
                      (fn [rf]
                        (fn
                          ([] (rf))
                          ([acc] (rf acc))
                          ([acc x]
                           (let [step (binding [*current-emitter* emitter]
                                        (xform x))]
                             (if (fn? step)
                               (step rf acc)
                               step))))))]

        (trans method))))

  (add-method [_ k method]
    (StatelessProcess. (assoc method-map k method) ::uninitialised)))

(deftype TransducerProcess [method-map]
  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

  Multiplexer
  (method [_ input]
    (get method-map input))
  (add-method [_ input method]
    (TransducerProcess. (assoc method-map input method))))

(defn stateful-process
  "Returns a process which operates on inputs in multiplexer and maintains
  internal state. Initial state is set to init-state if provided."
  ([multiplexer] (stateful-process nil multiplexer))
  ([init-state multiplexer]
   (stateful-process init-state init-state multiplexer))
  ([init-ev init-state multiplexer]
   (StatefulProcess. multiplexer init-ev init-state)))

(defn stateless-process
  "Returns a new process which operates on each input in the multiplexer map."
  [multiplexer]
  (StatelessProcess. multiplexer ::uninitialised))

(defn transducer-process
  "Returns a process which applies normal clojure transducers to input signals."
  ([evmap]
   (TransducerProcess. evmap))
  ([listen xform]
   (transducer-process {listen xform})))

(macros/deftime

  (defn maybe-varify
    "Return the var pointing at x if there is one, otherwise just return x."
    [x]
    (if (symbol? x)
      `(var ~x)
      x))

  (defmacro process
    "Constructs a new process.

  If only a method map is provided, it is assumed that the methods are Clojure
  transducers and a transducing process is returned. This is the most convenient
  form of process but limited in what it can do.

  N.B.: Using stateful Clojure transducers is not recommended since that will
  make saving and restoring application state (as in undo/redo, or pause/resume)
  impossible (the state in the closure cannot be saved or restored.

  If two arguments are given, the first must be a bindings vector and the second
  a map from inputs to method bodies.

  If one argument is present in bindings a stateless process will be created, if
  there are two arguments it is assumed that the first is local state and the
  second the event. In this case a stateful process will be created."
    ([method-map]
     (let [var-mm (into {} (map (fn [[k v]]
                                  `[~(maybe-varify k) ~v]))
                        method-map)]
       `(transducer-process ~var-mm)))
    ([bindings method-map]
     `(process nil {} ~bindings ~method-map))
    ([opts bindings method-map]
     `(process (:init-state ~opts) ~opts ~bindings ~method-map))
    ([init-state {:keys [wrap-body]} bindings method-bodies]
     (when (and (= 1 (count bindings)) init-state)
       (throw (Exception. (str "You're trying to create a stateless process with"
                               " initial state. That's a mistake!!!"))))
     (let [method-map (into {} (map (fn [[k v]]
                                      `[~(maybe-varify k)
                                        ~(if wrap-body
                                           `(~wrap-body (fn ~bindings ~v))
                                           `(fn ~bindings ~v))])
                                    method-bodies))]
       (if (= 2 (count bindings))
         `(stateful-process ~init-state ~method-map)
         `(stateless-process ~method-map)))))

  (defmacro defprocess
    "Creates a new process and binds it to n."
    {:style/indent [1]}
    [n doc? & args]
    (let [docstr (if (string? doc?) doc? nil)
          args   (if docstr args (cons doc? args))
          {:keys [reloaded? init-state] :as opts}
          (if (= 3 (count args)) (first args) {})]
      (if reloaded?
        (let [state    (symbol (str (name n) "-state$ubik"))
              listener (symbol (str (name n) "-listener$ubik"))]
          `(do
             (defonce ~state (atom ~init-state))
             (let [opts# (assoc ~opts :init-state @~state)])
             (def ~n
               ~@(when docstr [docstr])
               (process @~state ~opts ~@(rest args)))
             ;; TODO: Reloading with preserved state in clj.
             (macros/case :cljs
               (defonce ~listener
                 (.addEventListener
                  js/document.body "figwheel.before-js-reload"
                  (fn [e#]
                    (reset! ~state (get-state ~n))))))))
        `(def ~n
           ~@(when docstr [docstr])
           (process ~@args))))))

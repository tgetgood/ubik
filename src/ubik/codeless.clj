(ns ubik.codeless
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [ubik.codebase :as code]
            [ubik.storage :as store]
            [ubik.topology :as topo]))

(def built-in-code
  (quote
   [{::edits (fn [ev]
              (:text ev))

     ::form {:edit (fn [prev text]
                    (try
                      {:emit (read-string text)}
                      (catch Exception e {:unreadable text})))}

     ::display (fn [branch sym]
                (fn [image]
                  (get image (name sym))))

     ::format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    ;; Runtimey things
    {:editor.core/image-signal image-signal}]))

(def initial-topology
  {:inputs    {}
   :effectors {}
   :nodes     {}
   :edges     #{}})

(def snip-edit-topology
  "Creates an editor window and returns a messaging topology to control it."
  (code/ref-sig {display          :core/display
                 format-code-text :core/format-code-text
                 edits            :core/edits
                 form             :core/form}
    (fn [branch sym]
      (let [stage        (ubik.core/create-code-stage branch sym)
            key-strokes  (-> stage :event-streams :key-stroke)
            text-obj     (-> stage :node)
            code-display (display branch sym)
            text-render  (ubik.core/text-renderer text-obj)
            code-change  (ubik.core/source-effector branch sym)]
        {:topology
         {
          ;; The nodes in a topology are distict process fragments. One
          ;; function or multiplexer map can be instantiated into
          ;; multiple nodes in the graph, each with different internal
          ;; state and different connections. The same computation can
          ;; mean different things in different contexts.
          :nodes {::code-1 (map code-display)
                  ::code-2 (map format-code-text)
                  ::edits  (map edits)
                  ::form   (ubik.core/make-node form)}

          ;; I'm not sure that we need to explicitely declare sources
          ;; and sinks, but right now it's just easier this way.
          :sources {::image       ubik.core/image-signal
                    ::key-strokes key-strokes}

          :sinks {::text-render text-render
                  ::code-change code-change}

          ;; Wires connect a set of named inputs to a node. Each name in
          ;; the input map is assumed to also be the name of an input
          ;; signal to the node. If it is not, it will be
          ;; ignored. Similarly, not all signals a node can listen for
          ;; need to be connected. Whether the node can do anything of
          ;; use without all of its signals is application logic.
          ;; Currently, I'm requiring the wiring diagram to be pure
          ;; data, but I'm allowing the nodes to be compiled things. I
          ;; don't think that's ideal, but I don't know how to resolve
          ;; that yet.  Is the right thing to force all of the local
          ;; bindings to be effectively global, and then refer to the
          ;; snippets that will be converted into runtime constructs by
          ;; id? That seems extreme. But maybe extremism is called
          ;; for...
          :wires #{[{:in ::image} ::code-1]
                   [{:in ::code-1} ::code-2]
                   [::code-2 ::text-render]

                   [{:in ::key-strokes} ::edits]
                   [{:edit ::edits} ::form]
                   [::form ::code-change]}}}))))

(defn go []
  (code/clear-ns code/*primary-ns*)
  (code/load-ns (store/as-map code/*store*))
  (let [init (get (ns-interns code/*primary-ns*)
                  (code/interned-var-name
                   #uuid "6e901074-482d-408e-b495-ed8eae654043"))
        top (init "master" :stm)]
    (topo/init-topology! :test (:topology top))))

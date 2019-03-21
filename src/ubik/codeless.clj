(ns ubik.codeless
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [ubik.codebase :as code]
            [ubik.codebase.storage :as store]
            [ubik.rt :as rt]
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

(code/snippet {}
  (fn [image syms]
    (into {} (map (fn [s]
                    (let [ref (get-in image [(namespace s) (name s)])]
                      [(:ns/symbol ref)
                       (invoke-by-id (:id ref))])))
                      syms)))

(def extract-deps
  "I've only named these as vars for the ease of reference"
  (code/snippet {fn-map #uuid "e03eccd4-f7ae-43ae-a107-97d97eafa255"}
    (fn [image]
      (fn-map image [:core/display
                     :core/format-code-text
                     :core/edits
                     :core/form]))))

(def edit-multi
  "Multiplexer that takes inputs from two signals and produces a new signal
  which emits the combination each time either input changes."
  (code/snippet {}
    {:image (fn [{:keys [watch] :as state} image]
              (let [s' (assoc state :image image)]
                (if watch
                  (assoc s' :emit s')
                  s')))
     :watch (fn [{:keys [image] :as state} watch]
             (let [s' (assoc state :watch watch)]
               (if image
                 (assoc s' :emit s')
                 s')))}))

(def snip-edit-topology
  "Creates an editor window and returns a messaging topology to control it."
  (code/snippet {}
    (fn [{{:keys [display format-code-text edits form]} :image
          watch                                         :watch}]
      (let [stage        (ubik.core/create-code-stage watch)
            key-strokes  (-> stage :event-streams :key-stroke)
            text-obj     (-> stage :node)
            code-display (display watch)
            text-render  (ubik.core/text-renderer text-obj)
            code-change  (ubik.core/source-effector watch)]
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
                  [::form ::code-change]}}))))

(def meta-topo
  (code/snippet {edit-multi   #uuid "df9b93b3-7431-4049-8008-80248c292491"
                 extract-deps #uuid "8dd1928f-ffac-4471-b8fa-7085186a080e"
                 topo-fac     #uuid "59476105-595a-44ac-a905-e184b0c2d213"}
    {:sources {::image ubik.core/image-signal
               ::input ubik.core/input-signal}
     :sinks   {::out ubik.core/topo-effector}
     :nodes   {::sub-image (map extract-deps)
               ::combined  (ubik.core/make-node edit-multi)
               ::topo      (map topo-fac)}
     :wires   #{[::image ::sub-image]
                [{:image ::sub-image :watch ::input} ::combined]
                [::combined ::topo]
                [::topo ::out]}}))

(defn trigger-network
  "Set off a cascade that should result in something interesting happening. I'm
  becomming less and less discerning in what I consider interesting."
  []
  (code/reload!)
  (topo/init-topology!
   :pre-boot
   (code/invoke-by-id #uuid "7ab9c4d0-78b4-43db-a800-da8b7dd9ffc1"))
  (rt/send code/image-signal (code/internal-ns-map)))

(defn sources []
  (-> topo/running-topologies
      deref
      :pre-boot
      :sources))

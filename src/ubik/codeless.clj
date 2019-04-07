(ns ubik.codeless
  (:require [falloleen.core :as falloleen]
            [falloleen.hosts :as hosts]
            [ubik.codebase :as code :refer [sdef]]
            [ubik.res.code-gen :as gen]
            [ubik.codebase.storage :as store]
            [ubik.codebase.config :as config]
            [ubik.process :as process]
            [ubik.topology :as topo]))

(sdef edits
  (code/snippet {}
    (fn [ev]
      (:text ev))))

(sdef form
  (code/snippet {}
    {:edit (fn [prev text]
             (try
               {:emit (read-string text)}
               (catch Exception e {:unreadable text})))}))

(sdef format-code-text
  (code/snippet {}
    (fn [form]
      (with-out-str (pprint form)))))

(sdef ns-ref
  (code/snippet {}
    (fn [image sym]
      (get-in image [(namespace sym) (name sym)]))))

(sdef fn-snippet
  (code/snippet {ns-ref "6a61ed710da21fd941d5eb6d45320fab2a3f58f4"}
    (fn [image sym]
      (let [ref (ns-ref image sym)]
        [(:name ref)
         (edit (:ref ref))]))))

(sdef display
  (code/snippet {lookup :core/fn-snippet}
    (fn [sym]
      (fn [image]
        (second (lookup image sym))))))

(sdef ns-lookup
  (code/snippet {ns-ref "6a61ed710da21fd941d5eb6d45320fab2a3f58f4"}
    (fn [image sym]
      (let [ref (ns-ref image sym)]
        [(:name ref) (invoke-by-id (:ref ref))]))))

(sdef fn-map
  (code/snippet {ns-lookup :core/ns-lookup}
    (fn [image syms]
      (into {} (map (partial ns-lookup image)) syms))))

(sdef extract-deps
  "I've only named these as vars for the ease of reference"
  (code/snippet {fn-map :core/fn-map}
    (fn [image]
      (fn-map image [:core/display
                     :core/format-code-text
                     :core/edits
                     :core/form]))))

(sdef edit-multi
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

(sdef snip-edit-topology
  "Creates an editor window and returns a messaging topology to control it."
  (code/snippet {}
    (fn [{{:keys [core/display
                  core/format-code-text
                  core/edits
                  core/form]} :image
          watch               :watch}]
      (let [stage        (create-code-stage watch)
            key-strokes  (-> stage :event-streams :key-stroke)
            text-obj     (-> stage :node)
            code-display (display watch)
            text-render  (text-renderer text-obj)
            code-change  (source-effector watch)]
        {
         ;; The nodes in a topology are distict process fragments. One
         ;; function or multiplexer map can be instantiated into
         ;; multiple nodes in the graph, each with different internal
         ;; state and different connections. The same computation can
         ;; mean different things in different contexts.
         :nodes [(process :ed/code-1 (map code-display))
                 (process :ed/code-2 (map format-code-text))
                 (process :ed/edits (map edits))
                 (make-node :ed/form form)
                 key-strokes
                 (effector :ed/text-render text-render)
                 (effector :ed/code-change code-change)]

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
         :wires #{[:ubik.topology/image :ed/code-1]
                  [:ed/code-1 :ed/code-2]
                  [:ed/code-2 :ed/text-render]

                  [[:ubik.events/text-area watch :key-stroke] :ed/edits]
                  [{:edit :ed/edits} :ed/form]
                  [:ed/form :ed/code-change]}}))))

(sdef meta-topo
  (code/snippet {edit-multi   :core/edit-multi
                 extract-deps :core/extract-deps
                 topo-fac     :core/snip-edit-topology}

    {:nodes [(signal :mt/input)
             (process :mt/sub-image (map extract-deps))
             (make-node :mt/combined edit-multi)
             (process :mt/topo (map topo-fac))
             (effector :mt/out topo-effector)]

     :wires #{[:ubik.topology/image :mt/sub-image]
              [{:image :mt/sub-image :watch :mt/input} :mt/combined]
              [:mt/combined :mt/topo]
              [:mt/topo :mt/out]}}))

(defn init []
  (gen/reload!)
  (topo/destroy!)
  (topo/init-topology! (gen/invoke-head :core/meta-topo)))

(defn edit
  "Open editor for sym."
  [sym]
  (process/send (:mt/input (:nodes @topo/topology)) sym))

(sdef topology-visual
  (code/snippet {}
    (fn [{:keys [nodes wires]}]
      (falloleen/translate
       (let [c (assoc (falloleen/circle :radius 10))]
         (mapv #(falloleen/translate c [% 0])
               (map #(* % 30) (range (count nodes)))))
       [0 200]))))

(sdef tvt
  (code/snippet {tv :core/topology-visual}
    {:nodes [(process ::tv (map tv))]
     :wires [[::topo/topology ::tv]
             [::tv ::topo/screen]]}))

(defn go-top! []
  (topo/init-topology! (gen/invoke-head :core/tvt)))

(sdef screen
  (code/snippet {}
    (-> f/circle
        (assoc :radius 40)
        (f/style {:opacity 0.4 :fill :pink}))))

(sdef structural-edit-topology
  (code/snippet {screen :core/screen}
    {:nodes []}))

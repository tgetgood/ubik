(ns ubik.codeless
  (:require [ubik.codebase :as code]
            [ubik.codebase.internal :as internal]
            [ubik.codebase.storage :as store]
            [ubik.codebase.config :as config]
            [ubik.process :as process]
            [ubik.topology :as topo]))

(def built-in-code
  (quote
   [{::edits (fn [ev]
              (:text ev))

     ::form {:edit (fn [prev text]
                    (try
                      {:emit (read-string text)}
                      (catch Exception e {:unreadable text})))}


     ::format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    ]))

(def ns-ref
  (code/snippet {}
    (fn [image sym]
      (get-in image [(namespace sym) (name sym)]))))

(def fn-snippet
  (code/snippet {ns-ref #uuid "0ab1aee1-8704-45e0-a493-5a36fa65032c"}
    (fn [image sym]
      (let [ref (ns-ref image sym)]
        [(:ns/symbol ref)
         (edit (:id ref))]))))

(def display
  (code/snippet {lookup {:ref  :core/fn-snippet
                         :time #inst "2019-03-26T13:47:52.890-00:00"}}
    (fn [sym]
      (fn [image]
        (second (lookup image sym))))))

(def ns-lookup
  (code/snippet {ns-ref #uuid "0ab1aee1-8704-45e0-a493-5a36fa65032c"}
    (fn [image sym]
      (invoke-by-id (:id (ns-ref image sym))))))

(def fn-map
  (code/snippet {ns-lookup #uuid "329b8d22-fd5d-492e-b722-f58aed8282f8"}
    (fn [image syms]
      (into {} (map (partial ns-lookup image)) syms))))

(def extract-deps
  "I've only named these as vars for the ease of reference"
  (code/snippet {fn-map {:ref :core/fn-map
                         :time #inst "2019-03-26T13:49:23.948-00:00"}}
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

(def meta-topo
  (code/snippet {edit-multi   {:ref  :core/edit-multi
                               :time #inst "2019-03-26T13:13:23.628-00:00"}
                 extract-deps {:ref  :core/extract-deps
                               :time #inst "2019-03-26T13:49:23.948-00:00"}
                 topo-fac     {:ref  :core/snip-edit-topology
                               :time #inst "2019-03-26T13:13:23.628-00:00"}}

    {:nodes [(signal :mt/input)
             (process :mt/sub-image (map extract-deps))
             (make-node :mt/combined edit-multi)
             (process :mt/topo (map topo-fac))
             (effector :mt/out topo-effector)]

     :wires #{[:ubik.topology/image :mt/sub-image]
              [{:image :mt/sub-image :watch :mt/input} :mt/combined]
              [:mt/combined :mt/topo]
              [:mt/topo :mt/out]}}))


(def starting-ns
  {:core/display            (:id display)
   :core/format-code-text   #uuid "05abc4d9-9c43-4c69-bf43-48d91d714eb5"
   :core/edits              #uuid "a179a2af-0243-4f47-96fc-3cc2199ec05e"
   :core/form               #uuid "5c3d7b4d-b616-44d8-b6be-f877fd1711df"
   :core/ns-lookup          (:id ns-lookup)
   :core/fn-map             (:id fn-map)
   :core/extract-deps       (:id extract-deps)
   :core/edit-multi         (:id edit-multi)
   :core/snip-edit-topology (:id snip-edit-topology)
   :core/meta-topo          (:id meta-topo)
   :core/fn-snippet         (:id fn-snippet)})

(code/populate-nses starting-ns)

(defn trigger-network
  "Set off a cascade that should result in something interesting happening. I'm
  becomming less and less discerning in what I consider interesting."
  []
  ;; Refresh all residential code
  (internal/clear-ns)
  (internal/load-ns)

  ;; Setup topology
  (topo/init-topology! (internal/invoke-head :core/meta-topo))

  ;; Spoof input
  (process/send (:mt/input @topo/node-map) :core/display)
  (process/send (:ubik.topology/image @topo/node-map) (code/internal-ns-map)))

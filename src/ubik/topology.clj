(ns ubik.topology
  (:require clojure.reflect
            [clojure.set :as set]
            [ubik.rt :as rt]
            [ubik.util :refer [vmap]]))

(defn max-args
  "Returns the maximal non-var-args arity of the function f."
  [f]
  (transduce (comp (filter #(= (:name %) 'invoke))
                   (map :parameter-types)
                   (map count))
             max
             0
             (:members (clojure.reflect/reflect f))))

(defn make-stateful-node [mm]
  (let [state (atom nil)]
    (with-meta
      (vmap (partial rt/stateful-xform state) mm)
      {:state state})))

(defn make-node [method-map]
  (let [c (into #{} (map max-args) (vals method-map))]
    (assert (= 1 (count c)) "All methods of a node must have the same arity.")
    (cond
      (= #{1} c) (vmap rt/stateless-xform method-map)
      (= #{2} c) (make-stateful-node method-map)
      :else      (throw (Exception. "Unknown node method type.")))))

(defn set-topology! [t])
(defn current-topology [])

(defn replace-topology! [t]
  (set-topology! t)
  (activate! t))

(defn add-micro-topo [{:keys [nodes wires]}]
  (let [current (current-topology)
        new {:nodes (merge (:nodes current) nodes)
             :wires (set/union (:wires current) wires)}]
    (replace-topology! new)))

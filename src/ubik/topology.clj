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

(defonce running-topologies
  (atom {}))

(defn init-topology! [k {:keys [sinks sources nodes wires] :as t}]
  (let [nodes (into {} (map (fn [[k v]] [k (rt/process k v)])) nodes)
        sinks (into {} (map (fn [[k v]] [k (rt/effector k v)])) sinks)
        all (merge sources sinks nodes)]
    (doseq [[k v] wires]
      (let [k (if (keyword? k) {:in k} k)
            k (vmap #(get all %) k)
            v (get all v)]
        (run! #(rt/wire v (key %) (val %)) k)))
    (swap! running-topologies assoc k
           (assoc t :nodes nodes :sinks sinks))))

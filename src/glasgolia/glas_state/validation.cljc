(ns glasgolia.glas-state.validation
  (:require
    [glasgolia.glas-state.stateless :refer [combine-name]]
    [clojure.string :as str]))


(def warning-checks {})

(defn valid-node-id? [node-id]
  (and (or (keyword? node-id)
           (string? node-id)
           (number? node-id))
       (not (str/blank? (str node-id)))))


(def error-checks
  {:root-must-have-valid-id
   (fn [n id] (not (valid-node-id? id)))
   :a-node-must-be-a-map
   (fn [n _] (not (map? n)))
   :incorrect-node-type
   (fn [n _] (not (or (nil? (:type n)) (get #{:atomic :compound :history :parallel :final} (:type n)))))
   :initial-node-not-found
   (fn [n _]
     (when-let [initial (:initial n)]
       (if (get-in n (:states initial))
         {:context initial}
         nil)))
   :atomic-or-final-must-be-leaf-node
   (fn [n _]
     (when (contains? #{:atomic :final} (:type n))
       (not-empty (:states n))))


   })


(defn- execute-node-checks [full-id node check-map]
  (filter identity (map (fn [[msg check-fn]]
                          (when-let [error (check-fn node full-id)]
                            (merge {:node-id  full-id
                                    :error msg}
                                   (when-let [context (:context error)] {:context context}))))
                        check-map)))


(defn- validate-node [full-id node]
  (let [errors (execute-node-checks full-id node error-checks)
        warnings (execute-node-checks full-id node warning-checks)
        this-result {:warnings (into [] warnings)
                     :errors   (into [] errors)}]
    (if (not-empty errors)
      this-result
      (let [result-children (map (fn [child-id child-node]
                                   (let [child-full-id (combine-name full-id child-id)]
                                     (validate-node child-full-id child-node))) (:states node))]
        (reduce (fn [l r] {:warnings [] :errors []} {:warnings (into [] (concat (:warnings l) (:warnings r)))
                                                     :errors   (into [] (concat (:errors l) (:warnings r)))}) this-result result-children)))
    ))

(comment
  (validate-node "root" {})
  (validate-node "" {})
  (validate-node "root" {:id     :test
                         :type   :atomic
                         :states {:a :b}})
  )


(defn validate-machine [machine-def]
  (validate-node (:id machine-def) machine-def))

(comment
  (validate-machine {:id :test})
  (validate-machine {})
  (validate-machine {:id :test :initial :start})
  (validate-machine {:id :test :type :atomic :states {:a :b}})
  )

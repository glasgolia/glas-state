(ns glasgolia.glas-state.validation
  (:require [glasgolia.glas-state.stateless :refer [combine-name]]))


(def warning-checks {})

(def error-checks
  {:a-node-must-be-a-map #(map? %)

   })

(defn- validate-node [full-id node]
  (let [errors (filter identity (map (fn [[msg check-fn]] (when (not (check-fn node)) [full-id msg])) error-checks))
        warnings (filter identity (map (fn [[msg check-fn]] (when (not (check-fn node)) [full-id msg])) warning-checks))
        result-children (map (fn[child-id child-node]
                               (let [child-full-id (combine-name full-id child-id)]
                                 (validate-node child-full-id child-node))) (:states node))
        this-result {:warnings (into [] warnings)
                     :errors   (into [] errors)}
        ]
    (reduce (fn[l r ] {:warnings (into [] (concat (:warnings l) (:warnings r)))
                       :errors   (into [] (concat (:errors l) (:warnings r)))}) this-result result-children)))

(comment
  (validate-node "root" nil)
  (validate-node "root" [])
  (validate-node "root" {})
  (validate-node "" {})
  )


(defn validate-machine [machine-def]
  (let [root-id (:id machine-def "")
        result (validate-node (:id machine-def "") machine-def)]
    (if (not= root-id "")
      result
      {:errors (conj (:errors result) ["" :root-node-must-have-id])
      :warnings (:warnings result)})))

(comment
  (validate-machine {:id :test})
  (validate-machine nil)

  )

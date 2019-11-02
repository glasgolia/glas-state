(ns glasgolia.glas-state.stateless)

; Stateless Statechart implementation


(defn event-type [event]
  "Get the event-type for a given event"
  (cond (map? event) (:type event)
        (vector? event) (first event)
        (keyword? event) event
        (number? event) event
        :else (str event)))

(defn- action-array [action]
  "Makes it easy to merge an action list with new actions"
  (if (vector? action)
    (into [] (filter identity (flatten (map action-array action))))
    (if nil
      []
      [action])))

(defn node-type [node]
  "Returns the type of a node: :leaf :branch or :parallel"
  (cond
    (nil? (:states node)) :leaf
    (= (:type node) :parallel) :parallel
    :else :branch
    ))


(declare create-initial-transition-state)

(defn state-id-to-str [id]
  (cond
    (string? id) id
    (keyword? id) (subs (str id) 1)))


(defn combine-name [parent-name child-name]
  "Combines a  parent and child node name.
  ex: (combine-name \"parent\" :child) => \"parent.child\""
  (str parent-name "." (state-id-to-str child-name)))

(defn- relative-id? [id]
  (= (first (name id)) \.))

(defn make-id-absolute [id]
  (cond
    (string? id) (subs id 1)
    (keyword? id) (keyword (namespace id)(subs (name id) 1))))

(defn assign [context-update-fn]
  "Creates an event that will assign a new context value using the context-update-fn.
   You should only use this in the machine definition.
   The context function will be passed: the current context, the event.
   If the argument is not a function, than the function will be looked up in the
   machine config actions"
  {:type     :glas-state/assign-context
   :assigner context-update-fn})

(defn send-event
  "Creates an event that will send a new event to the current machine.
  You should only use this in the machine definition.
  The function expected an event as first argument, and an optional
  delay-context map as second argument"
  ([event delay-context]
   {:type          :glas-state/send
    :event         event
    :delay-context delay-context})
  ([event]
   {:type  :glas-state/send
    :event event}))

(defn done-event [name]
  (send-event (keyword "done" (str "." name))))

(declare final-node?)

(defn- leaf-final-node? [node]
  (= (:type node) :final))

(defn- branch-final-node? [node value]
  (let [child-nodes (:states node)
        child-node (get child-nodes value)]
    (leaf-final-node? child-node)))

(defn- parallel-final-node? [node value]
  (let [child-final-nodes (map (fn [[k v]] (final-node? (get node k) v)) value)]
    (every? true? child-final-nodes)))


(defn final-node? [node value]
  (case (node-type node)
    :leaf (leaf-final-node? node)
    :parallel (parallel-final-node? node value)
    :branch (branch-final-node? node value)))

(defn create-invoke-event [invoke-propertie]
  (assert (or (nil? invoke-propertie) (:id invoke-propertie)))
  (if invoke-propertie
    {:type :glas-state/invoke
     :config invoke-propertie}
    nil))
(defn create-invoke-cleanup-event [invoke-propertie]
  (if invoke-propertie
    {:type :glas-state/invoke-cleanup
     :id (:id invoke-propertie)}
    nil))

(defn- create-initial-leaf-transition-state [parent-name {:keys [entry invoke]}]
  (merge
    {:actions (action-array [entry (create-invoke-event invoke)])}
    ))

(defn create-initial-branch-transition-state [parent-name {:keys [initial states entry invoke] :as node}]
  (assert initial (str "Need an initial state for " node))
  (let [child-node (get states initial)
        child-result (create-initial-transition-state (combine-name parent-name initial) child-node)
        child-value (:value child-result)
        child-final? (leaf-final-node? child-node)
        done-action (when child-final? (done-event parent-name))]
    (merge {:value          (if child-value
                              {initial child-value}
                              initial)
            :on-done-events (action-array [done-action])
            :actions        (action-array [entry (create-invoke-event invoke) (:actions child-result) (:on-done-events child-result)])}))
  )


(defn create-initial-parallel-transition-state [parent-name {:keys [states entry invoke]}]
  (let [child-results (map (fn [[key child-node]]
                             [key (create-initial-transition-state nil child-node)]) states)
        child-values (into {} (map (fn [[k v]]
                                     [k (:value v)]) child-results))
        child-finals (into [] (map (fn [[key value]] (final-node? (get states key) value)) child-values))
        final? (every? true? child-finals)
        done-action (when final? (done-event parent-name))
        child-actions (action-array (map (fn [[_ v]] (:actions v)) child-results))
        child-done-actions (action-array (map (fn [[_ v]] (:on-done-events v)) child-results))]
    (merge
      {:value          child-values
       :on-done-events (action-array [done-action])
       :actions        (action-array [entry (create-invoke-event invoke) child-actions child-done-actions])}))
  )

(defn create-initial-transition-state
  [parent-name node]
  "This creates the initial transition-state for a given node"
  (case (node-type node)
    :leaf (create-initial-leaf-transition-state parent-name node)
    :parallel (create-initial-parallel-transition-state parent-name node)
    :branch (create-initial-branch-transition-state parent-name node)))



(defn machine-options
  "Create a new machine definition by merging the
  :guards, :actions, :activities :context and :service values
  in the options map"
  [machine options]
  (merge-with into machine {:guards     (:guards options)
                            :actions    (:actions options)
                            :activities (:activities options)
                            :context    (:context options)
                            :services   (:services options)})
  )


(defn start-machine [{:keys [context] :as machine}]
  (let [state (create-initial-transition-state "" machine)
        on-done (:on-done-events state)]
    (-> state
        (assoc :context context)
        (update :actions #(action-array [% on-done]))
        (dissoc :on-done-events))))

(defn evaluate-guard-condition [guards context event condition]
  (cond (fn? condition)
        (condition context event)
        :else ((get guards condition) context event)))

(defn find-valid-handler [event-handler guards context event]
  (cond (vector? event-handler) (let [mapped (map (fn [eh]
                                                    (find-valid-handler eh guards context event)) event-handler)]
                                  (first (filter identity mapped)))
        (map? event-handler) (if (:cond event-handler)
                               ;we have a guarded event-handler
                               (if (evaluate-guard-condition guards context event (:cond event-handler))
                                 event-handler
                                 nil)
                               event-handler)
        (nil? event-handler) nil
        :else {:target event-handler}
        ))


(defn child-id
  "Get the child id from a state value.
  This could be: {:a {:b :c} so this will return :a,
  or this could be just :a and this will return :a"
  [value]
  (if (map? value)
    (first (keys value))
    value))

(defn create-child-value [child-id child-value]
  (if child-value
    {child-id child-value}
    child-id))


(defn- get-event-handler [node guards context event]
  (let [event-handler (get-in node [:on (event-type event)])]
    (find-valid-handler event-handler guards context event)))


(declare create-transition-state)


(defn create-leaf-transition-state [node guards context event]
  (let [event-handler (get-event-handler node guards context event)
        new-target (:target event-handler)
        final? (= (:type node) :final)]
    (if final?
      {:handled true
       :actions []}
     (if event-handler
       (if new-target
         ;Not internal
         {:actions (action-array [(:actions event-handler) (create-invoke-cleanup-event (:invoke node)) (:exit node)])
          :target  new-target
          :handled true}
         ;Internal
         {:actions (action-array (:actions event-handler))
          :handled true})
       ;Not handled...
       {:actions (action-array [(:exit node) (create-invoke-cleanup-event (:invoke node))])
        :handled false}))
    ))

(defn create-branch-transition-state [parent-name node guards value context event]
  (let [current-child-id (child-id value)
        states (:states node)
        current-child-def (get states current-child-id)
        current-child-value (get value current-child-id)
        current-child-result (create-transition-state (combine-name parent-name current-child-id)
                                                      current-child-def
                                                      guards
                                                      current-child-value
                                                      context
                                                      event)]
    (let [{:keys [handled target actions on-done-events]} current-child-result]
      (if handled
        ; Event is handled by child
        (if target
          ;Child handled the event and we now have a new target child
          (let [new-child-def (get states target)
                _ (assert new-child-def (str "Can't find child " target " for node " node))
                new-child-result (create-initial-transition-state (combine-name parent-name target) new-child-def)
                done-action (when (leaf-final-node? new-child-def) (done-event parent-name))]
            {:actions        (action-array [actions (:actions new-child-result) on-done-events (:on-done-events new-child-result)])
             :value          (create-child-value target (:value new-child-result))
             :on-done-events done-action
             :handled        true})
          ;Child handled the event and we have no new child on this level
          {:actions (action-array [actions on-done-events])
           :value   (create-child-value current-child-id (:value current-child-result))
           :handled true})
        ; We need to handle the event on this level
        (let [event-handler (get-event-handler node guards context event)
              child-exit-actions (:actions current-child-result)
              new-target (:target event-handler)]
          (if event-handler
            (if new-target
              ;Not internal
              (if (relative-id? new-target)
                (let[absolute-target (make-id-absolute new-target)
                     new-child-node (get states absolute-target)
                     _ (assert new-child-node (str "Can't find relative child " new-target " for node " node))
                     new-child-result (create-initial-transition-state (combine-name parent-name absolute-target) new-child-node)
                     done-action (when (leaf-final-node? new-child-node) (done-event parent-name))]
                  {:actions (action-array [actions (:actions event-handler) (:actions new-child-result) on-done-events (:on-done-events new-child-result)])
                   :value (create-child-value absolute-target (:value new-child-result))
                   :handled true})
                {:actions (action-array [(:actions event-handler) child-exit-actions (:exit node) (create-invoke-cleanup-event (:invoke node))])
                :target  new-target
                :value   value
                :handled true})
              ;Internal
              {:actions (action-array [(:actions event-handler) child-exit-actions])
               :value   value
               :handled true})
            ;Not handled...
            {:actions (action-array [child-exit-actions (:exit node) (create-invoke-cleanup-event (:invoke node))])
             :value   value
             :handled false})
          )
        )
      )))



(defn create-parallel-transition-state [parent-name node guards value context event]
  (let [child-node (:states node)
        child-results (into {} (map (fn [[k v]]
                                      [k (create-transition-state (combine-name parent-name k)
                                                                  (get child-node k) guards v context event)])
                                    value))

        child-values (into {} (map (fn [[k v]]
                                     [k (:value v)]) child-results))
        child-actions (action-array (mapv (fn [v] (:actions v)) (vals child-results)))
        child-done-actions (action-array (mapv (fn [v] (:on-done-events v)) (vals child-results)))

        child-finals (into [] (map (fn [[key value]] (final-node? (get child-node key) value)) child-values))
        final? (every? true? child-finals)
        done-action (when final? (done-event parent-name))
        handled? (some true? (map (fn [v] (:handled v)) (vals child-results)))]
    (if handled?
      (let []
        {:value          (into {} (map (fn [[k v]] [k (or (:value v) (get value k))]) child-results))
         :handled        true
         :on-done-events done-action
         :actions        (action-array [child-actions])
         })
      ;We have to handle the event on this level...
      (let [event-handler (get-event-handler node guards context event)
            new-target (:target event-handler)]
        (if event-handler
          (if new-target
            ;Not internal
            {:actions (action-array [child-actions (:actions event-handler) (:exit node) (create-invoke-cleanup-event (:invoke node)) child-done-actions])
             :target  new-target
             :value   value
             :handled true}
            ;Internal
            {:actions (action-array [child-actions (:actions event-handler) child-done-actions])
             :value   value
             :handled true})
          ;Not handled...
          {:actions (action-array [child-actions (:exit node) (create-invoke-cleanup-event (:invoke node)) child-done-actions])
           :value   value
           :handled false})
        ))))


(defn create-transition-state [parent-name node guards value context event]

  (case (node-type node)
    :leaf (create-leaf-transition-state node guards context event)
    :parallel (create-parallel-transition-state parent-name node guards value context event)
    :branch (create-branch-transition-state parent-name node guards value context event)))





(defn transition-machine [machine current-state event]
  (let [value (:value current-state)
        guards (:guards machine)
        context (:context current-state)
        tried-trans (create-transition-state "" machine guards value context event)]
    (if (:handled tried-trans)
      (-> tried-trans
          (dissoc :handled)
          (update :actions #(action-array [% (:on-done-events tried-trans)]))
          (dissoc :on-done-events))
      (do
        (println "The event was not handled..." event)
        (assoc current-state :actions [])))))

(defn value-to-ids
  ([parent-id value]
   (cond
     (keyword? value) #{(name parent-id) (combine-name parent-id value) (name value)}
     (string? value) #{parent-id (combine-name parent-id value) (name value)}
     (map? value) (let [child-names (into #{} (map (fn [[k v]]
                                                     (value-to-ids (combine-name parent-id k) v)) value))
                        all-child-names (into #{} (reduce concat #{} child-names))]
                    (-> all-child-names
                        (conj (name parent-id)))
                    )
     :else #{}))
  ([value]
   (value-to-ids "" value)))

(defn leaf-value-to-ids
  ([parent-id value]
   (cond
     (keyword? value) #{(combine-name parent-id value)}
     (string? value) #{(combine-name parent-id value)}
     (map? value) (let [sub-names (into #{} (map (fn [[k v]]
                                                   (leaf-value-to-ids (combine-name parent-id k) v)) value))
                        all-sub-names (into #{} (reduce concat #{} sub-names))]
                    all-sub-names
                    )
     :else #{}))
  ([value]
   (leaf-value-to-ids "" value)))


(comment
  )
(ns glasgolia.glas-state.stateless)



(defn event-type [event]
  "Get the event-type for a given event"
  (cond (map? event) (:type event)
        (vector? event) (first event)
        (keyword? event) event
        (number? event) event
        :else (str event)))

(defn- action-array [action]
  (if (vector? action)
    (into [] (filter identity (flatten (map action-array action))))
    (if nil
      []
      [action])))

(defn state-def-type [state-def]
  (cond
    (nil? (:states state-def)) :leaf
    (= (:type state-def) :parallel) :parallel
    :else :non-leaf
    ))


(declare create-initial-transition-state)

(defn sub-name [parent-name sub-name]
  (str parent-name "." (name sub-name)))



(defn assign [context-update-fn]
  {:type     :glas-state/assign-context
   :assigner context-update-fn})

(defn send-event
  ([event delay-context]
   {:type          :glas-state/send
    :event         event
    :delay-context delay-context})
  ([event]
   {:type  :glas-state/send
    :event event}))

(defn done-event [name]
  (send-event (keyword "done" (str "." name))))

(declare final-state?)

(defn- leaf-final-state? [state-def]
  (= (:type state-def) :final))

(defn- non-leaf-final-state? [state-def value]
  (let [sub-states (:states state-def)
        sub-state (get sub-states value)]
    (leaf-final-state? sub-state)))

(defn- parallel-final-state? [state-def value]
  (let [sub-final-states (map (fn [[k v]] (final-state? (get state-def k) v)) value)]
    (every? true? sub-final-states)))


(defn final-state? [state-def value]
  (case (state-def-type state-def)
    :leaf (leaf-final-state? state-def)
    :parallel (parallel-final-state? state-def value)
    :non-leaf (non-leaf-final-state? state-def value)))

(defn- create-initial-leaf-transition-state [parent-name {:keys [entry type]}]
  (merge
    {:actions (action-array [entry])}
    ))

(defn create-initial-non-leaf-transition-state [parent-name {:keys [initial states entry] :as state-def}]
  (assert initial (str "Need an initial state for " state-def))
  (let [sub-state-def (get states initial)
        sub-result (create-initial-transition-state (sub-name parent-name initial) sub-state-def)
        sub-value (:value sub-result)
        sub-final? (leaf-final-state? sub-state-def)
        done-action (when sub-final? (done-event parent-name))]
    (merge {:value          (if sub-value
                              {initial sub-value}
                              initial)
            :on-done-events (action-array [done-action])
            :actions        (action-array [entry (:actions sub-result) (:on-done-events sub-result)])}))
  )


(defn create-initial-parallel-transition-state [parent-name {:keys [states entry]}]
  (let [sub-results (map (fn [[key sub-state]]
                           [key (create-initial-transition-state nil sub-state)]) states)
        sub-values (into {} (map (fn [[k v]]
                                   [k (:value v)]) sub-results))
        sub-finals (into [] (map (fn [[key value]] (final-state? (get states key) value)) sub-values))
        final? (every? true? sub-finals)
        done-action (when final? (done-event parent-name))
        sub-actions (action-array (map (fn [[_ v]] (:actions v)) sub-results))
        sub-done-actions (action-array (map (fn [[_ v]] (:on-done-events v)) sub-results))]
    (merge
      {:value          sub-values
       :on-done-events (action-array [done-action])
       :actions        (action-array [entry sub-actions sub-done-actions])}))
  )

(defn create-initial-transition-state
  [parent-name state-def]
  "This creates the initial transition-state for a given state-def"
  (case (state-def-type state-def)
    :leaf (create-initial-leaf-transition-state parent-name state-def)
    :parallel (create-initial-parallel-transition-state parent-name state-def)
    :non-leaf (create-initial-non-leaf-transition-state parent-name state-def)))


(defn default-catch-all-action [context event meta]
  (println "undefined-action called:" (:action meta)))

(defn machine
  ([machine-def options]
   {:machine-def      machine-def
    :guards           (:guards options)
    :actions          (:actions options)
    :activities       (:activities options)
    :context          (or (:context options) (:context machine-def) {})
    :catch-all-action (or (:catch-all-action options) default-catch-all-action)})
  ([machine-def]
   (machine machine-def {})))

(defn machine? [object]
  (not (nil? (:machine-def object))))

(defn state-def-machine
  "Get the State-Def for this machine"
  [machine]
  (:machine-def machine))

(defn start-machine [{:keys [machine-def context]}]
  (let [state (create-initial-transition-state "" machine-def)
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


(defn- get-event-handler [state-def guards context event]
  (let [event-handler (get-in state-def [:on (event-type event)])]
    (find-valid-handler event-handler guards context event)))


(declare create-transition-state)


(defn create-leaf-transition-state [state-def guards context event]
  (let [event-handler (get-event-handler state-def guards context event)
        new-target (:target event-handler)]
    (if event-handler
      (if new-target
        ;Not internal
        {:actions (action-array [(:actions event-handler) (:exit state-def)])
         :target  new-target
         :handled true}
        ;Internal
        {:actions (action-array (:actions event-handler))
         :handled true})
      ;Not handled...
      {:actions (action-array (:exit state-def))
       :handled false})
    ))

(defn create-non-leaf-transition-state [parent-name state-def guards value context event]
  (let [current-child-id (child-id value)
        states (:states state-def)
        current-child-def (get states current-child-id)
        current-child-value (get value current-child-id)
        current-child-result (create-transition-state (sub-name parent-name current-child-id)
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
                _ (assert new-child-def (str "Can't find child " target " for state-def " state-def))
                new-child-result (create-initial-transition-state (sub-name parent-name target) new-child-def)
                done-action (when (leaf-final-state? new-child-def) (done-event parent-name))]
            {:actions        (action-array [actions (:actions new-child-result) on-done-events (:on-done-events new-child-result)])
             :value          (create-child-value target (:value new-child-result))
             :on-done-events done-action
             :handled        true})
          ;Child handled the event and we have no new child on this level
          {:actions (action-array [actions on-done-events])
           :value   (create-child-value current-child-id (:value current-child-result))
           :handled true})
        ; We need to handle the event on this level
        (let [event-handler (get-event-handler state-def guards context event)
              child-exit-actions (:actions current-child-result)
              new-target (:target event-handler)]
          (if event-handler
            (if new-target
              ;Not internal
              {:actions (action-array [(:actions event-handler) child-exit-actions (:exit state-def)])
               :target  new-target
               :value   value
               :handled true}
              ;Internal
              {:actions (action-array [(:actions event-handler) child-exit-actions])
               :value   value
               :handled true})
            ;Not handled...
            {:actions (action-array [child-exit-actions (:exit state-def)])
             :value   value
             :handled false})
          )
        )
      )))



(defn create-parallel-transition-state [parent-name state-def guards value context event]
  (let [sub-state-defs (:states state-def)
        sub-results (into {} (map (fn [[k v]]
                                    [k (create-transition-state (sub-name parent-name k)
                                                                (get sub-state-defs k) guards v context event)])
                                  value))

        sub-values (into {} (map (fn [[k v]]
                                   [k (:value v)]) sub-results))
        sub-actions (action-array (mapv (fn [v] (:actions v)) (vals sub-results)))
        sub-done-actions (action-array (mapv (fn [v] (:on-done-events v)) (vals sub-results)))

        sub-finals (into [] (map (fn [[key value]] (final-state? (get sub-state-defs key) value)) sub-values))
        final? (every? true? sub-finals)
        done-action (when final? (done-event parent-name))
        handled? (some true? (map (fn [v] (:handled v)) (vals sub-results)))]
    (if handled?
      (let []
        {:value          (into {} (map (fn [[k v]] [k (or (:value v) (get value k))]) sub-results))
         :handled        true
         :on-done-events done-action
         :actions        (action-array [sub-actions])
         })
      ;We have to handle the event on this level...
      (let [event-handler (get-event-handler state-def guards context event)
            new-target (:target event-handler)]
        (if event-handler
          (if new-target
            ;Not internal
            {:actions (action-array [sub-actions (:actions event-handler) (:exit state-def) sub-done-actions])
             :target  new-target
             :value   value
             :handled true}
            ;Internal
            {:actions (action-array [sub-actions (:actions event-handler) sub-done-actions])
             :value   value
             :handled true})
          ;Not handled...
          {:actions (action-array [sub-actions (:exit state-def) sub-done-actions])
           :value   value
           :handled false})
        ))))


(defn create-transition-state [parent-name state-def guards value context event]
  (case (state-def-type state-def)
    :leaf (create-leaf-transition-state state-def guards context event)
    :parallel (create-parallel-transition-state parent-name state-def guards value context event)
    :non-leaf (create-non-leaf-transition-state parent-name state-def guards value context event)))





(defn transition-machine [machine current-state event]
  (let [value (:value current-state)
        machine-def (:machine-def machine)
        guards (:guards machine)
        context (:context current-state)
        tried-trans (create-transition-state "" machine-def guards value context event)]
    (if (:handled tried-trans)
      (-> tried-trans
          (dissoc :handled)
          (update :actions #(action-array [% (:on-done-events tried-trans)]))
          (dissoc :on-done-events))
      (do
        (println "The event was not handled...")
        (assoc current-state :actions [])))))

(defn value-to-ids
  ([parent-id value]
   (cond
     (keyword? value) #{(name parent-id) (sub-name parent-id value) (name value)}
     (string? value) #{parent-id (sub-name parent-id value) (name value)}
     (map? value) (let [sub-names (into #{} (map (fn [[k v]]
                                                   (value-to-ids (sub-name parent-id k) v)) value))
                        all-sub-names (into #{} (reduce concat #{} sub-names))]
                    (-> all-sub-names
                        (conj (name parent-id)))
                    )
     :else #{}))
  ([value]
   (value-to-ids "" value)))

(defn leaf-value-to-ids
  ([parent-id value]
   (cond
     (keyword? value) #{(sub-name parent-id value)}
     (string? value) #{(sub-name parent-id value)}
     (map? value) (let [sub-names (into #{} (map (fn [[k v]]
                                                   (leaf-value-to-ids (sub-name parent-id k) v)) value))
                        all-sub-names (into #{} (reduce concat #{} sub-names))]
                    all-sub-names
                    )
     :else #{}))
  ([value]
   (leaf-value-to-ids "" value)))


(comment
  (value-to-ids {:trafic-light :done
                 :style        {:bold :off, :underline :off}})
  (value-to-ids :a)
  (value-to-ids {:a :b})
  (value-to-ids :a)
  (value-to-ids {:a {:b :c
                     :d :e}})
  (leaf-value-to-ids {:a {:b :c
                          :d :e}})
  )
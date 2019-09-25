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

(defn- create-initial-leaf-transition-state [{:keys [entry]}]
  {:actions (action-array entry )})

(defn create-initial-non-leaf-transition-state [{:keys [initial states entry] :as state-def}]
  (assert initial (str "Need an initial state for " state-def))
  (let [sub-state-def (get states initial)
        sub-result (create-initial-transition-state sub-state-def)
        sub-value (:value sub-result)]
    {:value (if sub-value
              {initial sub-value}
              initial)
     :actions (action-array [entry (:actions sub-result)])})
  )



(defn create-initial-parallel-transition-state [{:keys [states entry]}]
  (let [ sub-results (map (fn [[key sub-state]]
                            [key (create-initial-transition-state sub-state)]) states)
        sub-values (into {} (map (fn[[k v]]
                                   [k (dissoc v :actions)]) sub-results))
        sub-actions (action-array (map (fn [[_ v]] (:actions v)) sub-results))]
    {:value sub-values
     :actions (action-array [entry sub-actions])})
  )


(defn create-initial-transition-state
  [state-def]
  "This creates the initial transition-state for a given state-def"
  #_(println "Creating initial state for " state-def)
  (case (state-def-type state-def)
    :leaf (create-initial-leaf-transition-state state-def)
    :parallel (create-initial-parallel-transition-state state-def)
    :non-leaf (create-initial-non-leaf-transition-state state-def)))

(defn machine
  ([machine-def options]
   {:machine-def machine-def
    :guards (:guards options)
    :actions (:actions options)
    :activities (:activities options)
    :context (or (:context options) (:context machine-def) {})})
  ([machine-def]
   (machine machine-def {})))

(defn start-machine [{:keys [machine-def context]}]
  (assoc (create-initial-transition-state machine-def) :context context))

(defn evaluate-guard-condition [guards context event condition]
  (cond (fn? condition)
        (condition context event)
        :else ((get guards condition) context event)))

(defn find-valid-handler [event-handler guards context event]
  (cond (vector? event-handler)
          ;We have a vector of multiple guarded event-handlers)
        (let [mapped (map (fn [eh]
                            (find-valid-handler eh guards context event)) event-handler)]
          (first (filter identity mapped)))
        (map? event-handler)
          (if (:cond event-handler)
            ;we have a guarded event-handler
            (if (evaluate-guard-condition guards context event (:cond event-handler))
               event-handler
               nil)
            event-handler)
        (nil? event-handler) nil
        :else {:target event-handler}
        ))

(defn create-transition-state-leaf [state-def guards value context event]
  (let [sub-state-defs (:states state-def)
        sub-state-def (get sub-state-defs value)
        event-handler (get-in sub-state-def [:on (event-type event)])]
    (if event-handler
      (let [internal? (or (not (:target event-handler)) (:internal event-handler))
            exit-handlers]
        {:target (:target event-handler)})
      {:actions (action-array (:exit sub-state-defs))})))

(defn create-transition-state [state-def guards value context event]
  (let [state-type (state-def-type state-def)
        current-value (:value state-type)]
    ))


(defn transition-machine [machine current-state event]
  (create-transition-state (:machine-def machine) current-state event))

(defn create-transition-state [state-def value event]
  (let [etype (event-type event)
        m-states (:states state-def)]
    (if (map? value)
      ; We have sub states
      (let [[state-id substate-id] (first value)
            sub-result (create-transition-state (get m-states state-id) substate-id event)]
        (if (:value sub-result)
          {:value   {state-id (:value sub-result)}
           :actions (:actions sub-result)}
          ;sub state did not handle the event, we have to handle it on this level
          (let [this-result (create-transition-state state-def state-id event)
                this-state (get m-states state-id)
                new-state (get m-states (:value this-result))]
            {:value   (:value this-result)
             :actions (into [] (concat (:actions sub-result) (:exit this-state) (:entry new-state)))})
          ))

      ; We are on a leaf state

      (let [this-m-state (get m-states value)
            this-on-event (get-in this-m-state [:on etype])
            this-exit-actions (:exit this-m-state)]
        (if this-on-event
          (let [new-state (get m-states this-on-event)
                entry-actions (:entry new-state)
                new-state-not-leaf? (:states new-state)]
            (if new-state-not-leaf?
              (let [init-result (create-initial-transition-state new-state)]
                {:value   {this-on-event (:value init-result)}
                 :actions (into [] (concat this-exit-actions entry-actions (:actions init-result)))})
              {:value   this-on-event
               :actions (into [] (concat this-exit-actions entry-actions))}))
          {:actions this-exit-actions})))))



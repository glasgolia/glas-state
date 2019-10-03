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

(defn- sub-name [parent-name sub-name]
  (str parent-name "." (name sub-name)))

(defn- done-event [name]
  (keyword "done" (str "." name) ))

(defn- create-initial-leaf-transition-state [parent-name {:keys [entry type]}]
  (let [done? (= type :final)
        done-action (when (and done? parent-name) (done-event parent-name))]
    (merge
      {:actions (action-array [entry done-action])}
      (when done? {:done true})) ))

(defn create-initial-non-leaf-transition-state [parent-name {:keys [initial states entry] :as state-def}]
  (assert initial (str "Need an initial state for " state-def))
  (let [sub-state-def (get states initial)
        sub-result (create-initial-transition-state parent-name sub-state-def)
        sub-value (:value sub-result)
        sub-done (:done sub-result)]
    (merge {:value   (if sub-value
                       {initial sub-value}
                       initial)
            :actions (action-array [entry (:actions sub-result)])}
           (when sub-done {:done true})
           ))
  )



(defn create-initial-parallel-transition-state [parent-name {:keys [states entry]}]
  (let [sub-results (map (fn [[key sub-state]]
                           [key (create-initial-transition-state nil sub-state)]) states)
        sub-done (map (fn [[_ v]] (:done v)) sub-results)
        done? (not (some nil? sub-done))
        done-action (when (and done? parent-name) (done-event parent-name))
        sub-values (into {} (map (fn [[k v]]
                                   [k (:value v)]) sub-results))
        sub-actions (action-array (map (fn [[_ v]] (:actions v)) sub-results))]
    (merge
      {:value   sub-values
      :actions (action-array [entry sub-actions done-action])}
      (when done? {:done true})))
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

(defn start-machine [{:keys [machine-def context]}]
  (assoc (create-initial-transition-state "" machine-def) :context context))

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

(defn create-transition-state-single-track [parent-name state-def guards value context event]
  (let [sub-state-defs (:states state-def)
        sub-state-def (get sub-state-defs value)
        exit-actions (:exit sub-state-def)
        event-handler (get-in sub-state-def [:on (event-type event)])
        event-handler (find-valid-handler event-handler guards context event)]
    (if event-handler
      (let [target (:target event-handler)
            new-value (or target value)
            event-actions (action-array (:actions event-handler))
            new-state-def (get sub-state-defs new-value)
            new-state-init (create-initial-transition-state (sub-name parent-name new-value) new-state-def)
            new-state-entry (:actions new-state-init)
            new-state-value (if-let [v (:value new-state-init)]
                              {new-value v}
                              new-value)]
        (if (= new-value value)
          ; The target is the same node...
          (let [internal? (or (not target) (:internal event-handler))]
            ;If internal don't return exit/entry actions
            (if internal?
              ; internal to self
              {:value new-state-value}
              ; to self, not internal
              {:value   new-state-value
               :actions (action-array [exit-actions new-state-entry event-actions])}))
          ; We have a new target, so return the value and exit/entry actions
          {:value   new-state-value
           :actions (action-array [exit-actions new-state-entry event-actions])}))
      ; We could not handle the event on this level, just return the exit actions
      {:actions (action-array exit-actions)})))

(declare create-transition-state)

(defn create-transition-state-multi-track [parent-name state-def guards value context event]
  (let [sub-state-defs (:states state-def)
        sub-results (into {} (map (fn [[k v]]
                                    [k (create-transition-state (sub-name parent-name k)
                                                                ( get sub-state-defs k) guards v context event)])
                                  value))
        sub-actions (action-array (into [] (map (fn [v] (:actions v)) (vals sub-results))))
        values? (not-empty (filter (fn [v] (:value v)) (vals sub-results)))]
    (if values?
      ;We have sub values, so we can stop...
      {:value   (into {} (map (fn [[k v]] [k (or (:value v) (get value k))]) sub-results))
       :actions sub-actions}
      ;We have to handle the event on this level...
      (let [all-sub-values (keys value)
            all-sub-result (map (fn [v] (create-transition-state-single-track (sub-name parent-name v)state-def guards v context event)) all-sub-values)
            all-sub-actions (into [] (reduce concat (map (fn [r] (:actions r)) all-sub-result)))
            this-result (first all-sub-result)
            this-result (assoc this-result :actions (action-array [sub-actions all-sub-actions]))]
        this-result))))

(defn create-transition-state [parent-name state-def guards value context event]
  (if (map? value)
    (create-transition-state-multi-track parent-name state-def guards value context event)
    (create-transition-state-single-track parent-name state-def guards value context event)))

(defn transition-machine [machine current-state event]
  (let [value (:value current-state)
        machine-def (:machine-def machine)
        guards (:guards machine)
        context (:context current-state)
        tried-trans (create-transition-state "" machine-def guards value context event)]
    (if (:value tried-trans)
      tried-trans
      (assoc current-state :actions []))))


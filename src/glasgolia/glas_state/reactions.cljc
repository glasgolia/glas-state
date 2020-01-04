(ns glasgolia.glas-state.reactions)




(defn create-reactor
  "Create a new reactor data structure.
  This data structure keeps track of the previous state
  en all the added reactors"
  []
  {:reactors   (atom [])
   :prev-state (atom {})})

(defn add-reaction
  "Add a new reaction to a reactor.
  The filter can be a function that filters the state,
  or a vector with a path in a map.
  The fun parameter is a function that accepts the previous
  and the new filtered state when the state is changed.
  The reaction will be called with the current state on
  adding."
  [reactor filter fun]
  (if (vector? filter)
    (recur reactor (fn [e] (get-in e filter)) fun)
    (let [r {:filter filter :reaction fun}]
      (swap! (:reactors reactor) conj r)
      (let [prev-state (filter @(:prev-state reactor))]
        (fun prev-state prev-state))
      r)))




(defn exec-reactions
  "Execute the reactions to a state change"
  [{:keys [reactors prev-state]} new-state]
  (when-not (= @prev-state new-state)
    (doseq [{:keys [filter reaction]} @reactors]
      (let [pfs (filter @prev-state)
            nfs (filter new-state)]
        (when-not (= pfs nfs)
          (reaction pfs nfs))))
    (reset! prev-state new-state)))

(defn create-reaction-transition
  "Create a statechart transition listener that
  fires the reactions on state changes"
  [reactor]
  (fn [state]
    (exec-reactions reactor state)))

(comment
  (def r (create-reactor))

  (react r [:value :test] (fn [old new] (println "old:" old "new" new)))

  (exec-reactions r {:value :init
                     :context :test })
  (exec-reactions r {:value {:test :init}})
  (exec-reactions r {:value {:test :bla}})
  )
;
;(defn- notify-reactions [reactions old-value new-value]
;  (doseq [[_key reaction] reactions]
;    (try
;      (reaction old-value new-value)
;      (catch #?(:clj  Exception
;                :cljs :default) e (ex-info "Exception while notifying reactors of stateschart service!"
;                                           {:prev-state old-value :new-state new-value} e))))
;  )
;
;(defn- add-reaction
;
;  ([reaction-atom reaction]
;   (let [reaction-key (gensym)]
;     (swap! reaction-atom conj [reaction-key reaction])
;     (fn []
;       (swap! reaction-atom (fn [l] (into [] (filter (fn [p] (not= (first p) reaction-key)) l)))))))
;  ([reaction reaction-atom transformer]
;   (if (vector? transformer)
;     (add-reaction reaction-atom reaction (fn [v] (get-in v transformer)))
;     (add-reaction reaction-atom (fn [old new]
;                                   (let [old (transformer old)
;                                         new (transformer new)]
;                                     (when (not= old new)
;                                       (reaction old new))))))))
;(defn add-value-reaction
;  "Add a reaction when there is a value state change.
; The reaction must be a function that takes the old value and the new value.
; The optional transformer can be a function that takes a value and transforms it,
; or a vector that will be used as the get-in path for values.
; This functions returns a function that can be called  to remove the reaction from the service."
;  ([service reaction]
;   (let [value (service-value service)]
;     (reaction value value))
;   (add-reaction (:value-reactions service) reaction))
;  ([service reaction transformer]
;   (let [value (transformer (service-value service))]
;     (reaction value value))
;   (add-reaction (:value-reactions service) reaction transformer)))
;
;(defn add-context-reaction
;  "Add a reaction when there is a context state change.
; The reaction must be a function that takes the old value and the new value.
; The optional transformer can be a function that takes a value and transforms it,
; or a vector that will be used as the get-in path for values.
; This functions returns a function that can be called  to remove the reaction from the service."
;  ([service reaction]
;   (let [value (service-context service)]
;     (reaction value value))
;   (add-reaction (:context-reactions service) reaction))
;  ([service reaction transformer]
;   (let [value (transformer (service-context service))]
;     (reaction value value))
;   (add-reaction (:context-reactions service) reaction transformer)))
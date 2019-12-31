(ns glasgolia.glas-state.reactions)

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
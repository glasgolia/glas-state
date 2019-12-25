(ns glasgolia.glas-state.gatom
  (:require [clojure.core.async :as ca]))

(defn create-reaction [transformer reaction-fn]
  (let [prev-value (atom nil)]
    (if (vector? transformer)
      (recur (fn [v] (get-in v transformer)) reaction-fn)
      (fn [old-value new-value]
        (let [new-transformed (transformer new-value)]
          (when (not= @prev-value new-transformed)
            (reset! prev-value new-transformed)
            (reaction-fn new-transformed)))))))




(def first-name-reaction (create-reaction [:name :first] (fn [value] (println "Got new value: " value))))



(def change-listeners (atom []))

(defn add-reaction [change-listeners reaction]
  (let [reaction-key (gensym)]
    (swap! change-listeners concat [reaction-key reaction])
    (fn []
      (swap! change-listeners (fn [l]
                                (into [] (filter (fn [p] (not= (first p) reaction-key)) l)))))))


(add-reaction change-listeners first-name-reaction)


#_(defn cursor [parent-atom path]
    (let [this-atom (atom (get-in @parent-atom path))]
      (add-watch parent-atom path (fn [key a old-state new-state]
                                    (let [cursor-old @this-atom
                                          cursor-new (get-in new-state path)]
                                      (when (not= cursor-old cursor-new)
                                        (println "update cursor to " cursor-new)
                                        (reset! this-atom (get-in new-state path))))))
      (add-watch this-atom path (fn [key a old-state new-state]
                                  (let [parent-old @parent-atom
                                        ]
                                    (println "update main to " (get-in @parent-atom path new-state))
                                    (swap! parent-atom assoc-in path new-state))))))
(defn cursor [parent-atom path]
  (let [this-atom (atom (get-in @parent-atom path))]
    (add-watch parent-atom path (fn [key a old-state new-state]
                                  (when (not= old-state new-state)
                                    (swap! this-atom (fn [cursor-old]
                                                       (let [cursor-new (get-in new-state path)]
                                                         (println "updating cursor to " cursor-new)
                                                         cursor-new))))
                                  ))
    (add-watch this-atom path (fn [key a old-state new-state]
                                (when (not= old-state new-state)
                                  (swap! parent-atom (fn [parent-old]
                                                       (let [parent-new (assoc-in parent-old path new-state)]
                                                         (println "update main to " parent-new)
                                                         parent-new))))))))
(def peter (atom {:name    {:first :peter :last :muys}
                  :address {:street "steendam" :number "110a"}}))


(def first-name (cursor peter [:name :first]))
(comment

  ;(reset! peter @peter)
  ;(swap! peter identity)
  (println @peter)
  ;#_(println @first-name)
  ;#_(reset! first-name "Peter")

  ;(swap! peter update-in [:name :first] "Peter")
  (swap! peter (fn [x] (assoc-in x [:name :first] :Peter)))

  (reset! first-name :DePeter)
  (reset! peter {:name {:first :Els}})
  @first-name

  (ca/go (println "test"))

  (def counter (atom 0))
  (def p1 (atom {:count 0}))
  (def c1 (cursor p1 [:count]))
  (def c2 (cursor p1 [:count]))

  (doseq [x (range 2)]
    (ca/go
      (swap! c1 inc)
      ))
  (reset! c2 0)
  @p1
  @c2

  (reset! counter2 0)
  )
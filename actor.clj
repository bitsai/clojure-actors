(ns actor)

(defrecord actor-content [state msg-fn])

(defn actor [state msg-fn]
  (agent (actor-content. state msg-fn)))

(defn- apply-msg-fn [actor-content msg sender]
  (let [{:keys [state msg-fn]} actor-content
        new-state (msg-fn msg state sender)]
    (assoc actor-content :state new-state)))

(defn send [actor & xs]
  (let [sender *agent*]
    (send-off actor apply-msg-fn xs sender)
    nil))

(def println-actor (actor
                    nil
                    (fn [msg _ _]
                      (apply println msg))))

(defn println [& xs]
  (apply actor/send println-actor xs))

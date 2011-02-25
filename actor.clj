(ns actor)

(defn actor [state msg-fn]
  (agent {:state state :msg-fn msg-fn}))

(defn apply-msg-fn [{:keys [state msg-fn] :as content} msg args sender]
  (let [new-state (msg-fn state msg args sender)]
    (assoc content :state new-state)))

(defn send-msg [actor msg & args]
  (send-off actor apply-msg-fn msg args *agent*)
  nil)

(def printer (actor nil (fn [_ xs _ _] (apply println xs))))

(defn actor-println [& xs]
  (send-msg printer xs))

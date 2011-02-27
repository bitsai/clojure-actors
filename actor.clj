(ns actor)

(defn actor [state msg-fn]
  (agent {:state state :msg-fn msg-fn}))

(defn- apply-msg-fn [{:keys [state msg-fn] :as content} msg sender]
  (let [new-state (msg-fn state msg sender)]
    (assoc content :state new-state)))

(defn send-msg [actor & xs]
  (send-off actor apply-msg-fn xs *agent*)
  nil)

(def printer (actor nil (fn [_ xs _] (apply println xs))))

(defn actor-println [& xs]
  (apply send-msg printer xs))

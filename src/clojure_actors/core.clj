(ns clojure-actors.core)

(def actors (atom {}))

(defn make [name state handler]
  (let [actor (agent {:state state :handler handler})]
    (swap! actors assoc name actor)))

(defn- invoke-handler [body msg]
  (let [{:keys [state handler]} body
        new-state (handler state msg)]
    (assoc body :state new-state)))

(defn send [name & xs]
  (let [actor (get @actors name)]
    (send-off actor invoke-handler (vec xs))
    nil))

(make :printer
      nil
      (fn [_ msg]
        (apply println msg)))

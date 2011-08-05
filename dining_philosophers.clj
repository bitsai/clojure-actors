(ns dining-philosophers
  (:require actor))

;; Implements the Chandy/Misra solution to 'Dining Philosophers'
;; http://en.wikipedia.org/wiki/Dining_philosophers

(def eating-time 300)
(def thinking-time 300)

(defn eaten [state]
  (let [{:keys [l-asked? r-asked?]} state]
    (cond (and l-asked? r-asked?) (assoc state
                                    :l-fork nil
                                    :r-fork nil
                                    :l-asked? false
                                    :r-asked? false)
          l-asked? (assoc state
                     :l-fork nil
                     :r-fork :dirty
                     :l-asked? false)
          r-asked? (assoc state
                     :l-fork :dirty
                     :r-fork nil
                     :r-asked? false)
          :else (assoc state
                  :l-fork :dirty
                  :r-fork :dirty))))

(defn msg-fn [msg state sender]
  (let [msg-type (first msg)
        {:keys [id l-phil r-phil l-fork r-fork l-asked? r-asked?]} state
        self *agent*]
    (case msg-type
      :add-state (let [additional-state (second msg)]
                   (merge state additional-state))
      :try-to-eat (do (when (not= :clean l-fork)
                        (actor/send l-phil :ask :right))
                      (when (not= :clean r-fork)
                        (actor/send r-phil :ask :left))
                      (when (= :clean l-fork r-fork)
                        (actor/send self :eat))
                      state)
      :eat (do (actor/println "philosopher" id "is eating")
               (Thread/sleep eating-time)
               (actor/send self :think)
               (when l-asked?
                 (actor/send l-phil :fork :right))
               (when r-asked?
                 (actor/send r-phil :fork :left))
               (eaten state))
      :think (do (actor/println "philosopher" id "is thinking")
                 (Thread/sleep thinking-time)
                 (actor/send self :try-to-eat)
                 state)
      :fork (let [from (second msg)]
              (case from
                :left (do (when (= :clean r-fork)
                            (actor/send self :eat))
                          (assoc state :l-fork :clean))
                :right (do (when (= :clean l-fork)
                             (actor/send self :eat))
                           (assoc state :r-fork :clean))))
      :ask (let [from (second msg)]
             (case from
               :left (if (= :dirty l-fork)
                       (do (actor/send l-phil :fork :right)
                           (assoc state :l-fork nil))
                       (assoc state :l-asked? true))
               :right (if (= :dirty r-fork)
                        (do (actor/send r-phil :fork :left)
                            (assoc state :r-fork nil))
                        (assoc state :r-asked? true)))))))

(defn set-neighbors [philosophers]
  (let [n (count philosophers)]
    (doseq [i (range n)]
      (let [pi-1 (nth philosophers (mod (dec i) n))
	    pi (nth philosophers i)
	    pi+1 (nth philosophers (mod (inc i) n))]
	(actor/send pi :add-state {:l-phil pi-1 :r-phil pi+1})))))

(defn set-forks [philosophers]
  (let [[p0 & ps] philosophers
        pn (last ps)]
    (actor/send p0 :add-state {:l-fork :dirty :r-fork :dirty})
    (doseq [pi (butlast ps)]
      (actor/send pi :add-state {:l-fork nil :r-fork :dirty}))
    (actor/send pn :add-state {:l-fork nil :r-fork nil})))

(defn dining-philosophers [n]
  (when (> n 1)
    (let [philosophers (for [i (range n)]
                         (actor/actor
                          {:id i
                           :l-asked? false
                           :r-asked? false}
                          msg-fn))]
      (set-neighbors philosophers)
      (set-forks philosophers)
      (doseq [p philosophers]
	(actor/send p :think)))))

(dining-philosophers 5)

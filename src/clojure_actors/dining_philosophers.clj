(ns clojure-actors.dining-philosophers
  (:require [clojure-actors.core :as actor])
  (:use [clojure.core.match :only [match]]))

;; Implements the Chandy/Misra solution to 'Dining Philosophers'
;; http://en.wikipedia.org/wiki/Dining_philosophers

(def thinking-time 100)
(def eating-time 100)

(defn eaten [state]
  (let [{:keys [meals asked?]} state
        new-forks  (into {} (for [[p p-asked?] asked?]
                              [p (when-not p-asked? :dirty)]))
        new-asked? (zipmap (keys asked?) (repeat false))]
    {:meals (dec meals) :forks new-forks :asked? new-asked?}))

(defn make-philosopher! [id meals phil1 fork1 phil2 fork2]
  (actor/make!
   id
   {:meals meals
    :forks  {phil1 fork1, phil2 fork2}
    :asked? {phil1 false, phil2 false}}
   (fn [{:keys [meals forks asked?] :as state} msg]
     (match msg
            [:think]     (do (actor/send! :printer id "thinking")
                             (Thread/sleep (* (rand-int 10) thinking-time))
                             (when (pos? meals)
                               (actor/send! id :get-forks))
                             state)
            [:get-forks] (do (doseq [[p fork] forks :when (not= :clean fork)]
                               (actor/send! p :ask id))
                             state)
            [:eat]       (do (actor/send! :printer id "eating")
                             (Thread/sleep (* (rand-int 10) eating-time))
                             (actor/send! id :think)
                             (doseq [[p p-asked?] asked? :when p-asked?]
                               (actor/send! p :fork id))
                             (eaten state))
            [:ask phil]  (if (= :dirty (forks phil))
                           (do (actor/send! phil :fork id)
                               (assoc-in state [:forks phil] nil))
                           (assoc-in state [:asked? phil] true))
            [:fork phil] (let [new-forks (assoc forks phil :clean)]
                           (when (every? #(= :clean %) (vals new-forks))
                             (actor/send! id :eat))
                           (assoc state :forks new-forks))))))

(defn dining-philosophers [n meals]
  (if (< n 3)
    "n must be >= 3"
    (do (make-philosopher! 0 meals (dec n) :dirty 1 :dirty)
        (doseq [id (range 1 (dec n))]
          (make-philosopher! id meals (dec id) nil (inc id) :dirty))
        (make-philosopher! (dec n) meals (- n 2) nil 0 nil)
        (doseq [id (range n)]
          (actor/send! id :think)))))

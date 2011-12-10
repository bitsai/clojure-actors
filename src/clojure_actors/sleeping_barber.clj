(ns clojure-actors.sleeping-barber
  (:require [clojure-actors.core :as actor])
  (:use [clojure.core.match :only [match]]))

;; Port of gparallelizer's 'Sleeping Barber' example
;; http://code.google.com/p/gparallelizer/wiki/ActorsExamples

(def queue-capacity 5)
(def serving-time 100)

(actor/make!
 :barber
 nil
 (fn [_ msg]
   (match msg
          [:enter cust] (do (actor/send! :printer "Barber serving" cust)
                            (Thread/sleep (* (rand-int 10) serving-time))
                            (actor/send! cust :done)
                            (actor/send! :waiting-room :next))
          [:wait] (actor/send! :printer "Barber sleeping"))))

(actor/make!
 :waiting-room
 {:queue [] :asleep? true}
 (fn [{:keys [queue asleep?] :as state} msg]
   (let [full? (= queue-capacity (count queue))]
     (match msg
            [:enter cust] (cond full?   (do (actor/send! cust :full)
                                            state)
                                asleep? (do (actor/send! :waiting-room :next)
                                            (assoc state
                                              :queue (conj queue cust)
                                              :asleep? false))
                                :else   (do (actor/send! cust :wait)
                                            (assoc state
                                              :queue (conj queue cust))))
            [:next] (if-let [cust (first queue)]
                      (do (actor/send! :printer "Customer" cust "seated")
                          (actor/send! :barber :enter cust)
                          (assoc state :queue (subvec queue 1)))
                      (do (actor/send! :barber :wait)
                          (assoc state :asleep? true)))))))

(defn make-customer! [id]
  (actor/make!
   id
   nil
   (fn [_ msg]
     (match msg
            [:full]  (actor/send! :printer "Customer" id "leaving")
            [:wait]  (actor/send! :printer "Customer" id "waiting")
            [:start] (actor/send! :printer "Customer" id "being served")
            [:done]  (actor/send! :printer "Customer" id "been served")))))

(defn sleeping-barber [n]
  (doseq [i (range n)]
    (make-customer! i)
    (actor/send! :waiting-room :enter i)
    (Thread/sleep serving-time)))

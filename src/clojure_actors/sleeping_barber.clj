(ns clojure-actors.sleeping-barber
  (:require [clojure-actors.core :as actor])
  (:use [clojure.core.match :only [match]]))

;; Adapted from gparallelizer's 'Sleeping Barber' example
;; http://code.google.com/p/gparallelizer/wiki/ActorsExamples

(def queue-capacity 2)
(def serving-time 100)

(actor/make
 :barber
 nil
 (fn [_ msg]
   (match msg
          [:sleep] (actor/send :printer "Barber sleeping")
          [:wake]  (actor/send :printer "Barber waking")
          [:serve cust] (do (actor/send :printer "Barber serving" cust)
                            (Thread/sleep serving-time)
                            (actor/send cust :done)
                            (actor/send :wait-room :next)))))

(actor/make
 :wait-room
 {:queue [] :sleeping? true}
 (fn [state msg]
   (let [{:keys [queue sleeping?]} state
         full? (= queue-capacity (count queue))]
     (match msg
            [:enter cust] (cond full? (do (actor/send cust :leave)
                                          state)
                                sleeping? (do (actor/send :barber :wake)
                                              (actor/send :wait-room :next)
                                              (assoc state
                                                :queue (conj queue cust)
                                                :sleeping? false))
                                :else (do (actor/send cust :wait)
                                          (assoc state
                                            :queue (conj queue cust))))
            [:next] (if-let [cust (first queue)]
                      (do (actor/send cust :sit)
                          (actor/send :barber :serve cust)
                          (assoc state :queue (subvec queue 1)))
                      (do (actor/send :barber :sleep)
                          (assoc state :sleeping? true)))))))

(defn make-customer [id]
  (actor/make
   id
   nil
   (fn [_ msg]
     (match msg
            [:enter] (do (actor/send :printer "Customer" id "entering")
                         (actor/send :wait-room :enter id))
            [:leave] (actor/send :printer "Customer" id "leaving")
            [:wait]  (actor/send :printer "Customer" id "waiting")
            [:sit]   (actor/send :printer "Customer" id "sitting")
            [:done]  (actor/send :printer "Customer" id "served")))))

(defn sleeping-barber [n]
  (actor/send :barber :sleep)
  (doseq [i (range n)]
    (make-customer i)
    (actor/send i :enter)
    (Thread/sleep (rand-int serving-time))))

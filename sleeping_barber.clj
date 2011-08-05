(ns sleeping-barber
  (:require actor))

;; Port of 'Sleeping Barber' example from gparallelizer wiki
;; http://code.google.com/p/gparallelizer/wiki/ActorsExamples

(def capacity 3)
(def processing-time 500)

(def barber
  (actor/actor
   nil
   (fn [msg _ sender]
     (let [msg-type (first msg)]
       (case msg-type
         :enter (let [customer (second msg)
                      cust-id (:state @customer)]
                  (actor/println "Barber: Processing customer" cust-id)
                  (actor/send customer :start)
                  (Thread/sleep processing-time)
                  (actor/send customer :done)
                  (actor/send sender :next))
         :wait (actor/println "Barber: No customers, sleeping"))))))

(def waiting-room
  (actor/actor
   {:queue [] :asleep? true}
   (fn [msg state sender]
     (let [msg-type (first msg)
           {:keys [queue asleep?]} state
           full? (= capacity (count queue))
           self *agent*]
       (case msg-type
         :enter (cond full? (do (actor/send sender :full)
                                state)
                      asleep? (do (actor/send self :next)
                                  (assoc state
                                    :queue (conj queue sender)
                                    :asleep? false))
                      :else (do (actor/send sender :wait)
                                (assoc state :queue (conj queue sender))))
         :next (if (seq queue)
                 (do (actor/send barber :enter (first queue))
                     (assoc state :queue (subvec queue 1)))
                 (do (actor/send barber :wait)
                     (assoc state :asleep? true))))))))

(defn customer [id]
  (actor/actor
   id
   (fn [msg _ _]
     (let [msg-type (first msg)]
       (case msg-type
         :enter (do (actor/println "Customer" id "is entering")
                    (actor/send waiting-room :enter))
         :full (actor/println "Customer" id "is leaving")
         :wait (actor/println "Customer" id "is waiting")
         :start (actor/println "Customer" id "is being served")
         :done (actor/println "Customer" id "has been served"))
       id))))

(defn sleeping-barber []
  (actor/send barber :wait)
  (doseq [i (range)]
    (actor/send (customer i) :enter)
    (Thread/sleep (rand-int (* 2 processing-time)))))

(sleeping-barber)

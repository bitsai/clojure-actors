(ns sleeping-barber
  (:use actor))

;; Port of 'Sleeping Barber' example from gparallelizer wiki
;; http://code.google.com/p/gparallelizer/wiki/ActorsExamples

(def *capacity* 3)
(def *processing-time* 500)

(def barber
  (actor
   nil
   (fn [_ [msg-type & args] sender]
     (case msg-type
	   :enter (let [customer (first args)
			id (@customer :state)]
		    (actor-println "Barber: Processing customer" id)
		    (send-msg customer :start)
		    (Thread/sleep *processing-time*)
		    (send-msg customer :done)
		    (send-msg sender :next))
	   :wait (actor-println "Barber: No customers. Sleeping.")
	   nil))))

(def waiting-room
  (actor
   {:queue nil :asleep? true}
   (fn [{:keys [queue asleep?] :as state}
	[msg-type & _]
	sender]
     (let [self *agent*
	   full? (= (count queue) *capacity*)]
       (case msg-type
	     :enter (cond
		     full? (do
			     (send-msg sender :full)
			     state)
		     asleep? (do
			       (send-msg self :next)
			       (assoc state
				 :queue (cons sender queue)
				 :asleep? false))
		     :else (do
			     (send-msg sender :wait)
			     (assoc state :queue (cons sender queue))))
	     :next (if (seq queue)
		     (do
		       (send-msg barber :enter (last queue))
		       (assoc state :queue (butlast queue)))
		     (do
		       (send-msg barber :wait)
		       (assoc state :asleep? true)))
	     state)))))

(defn customer [id]
  (actor
   id
   (fn [id [msg-type & _] _]
     (case msg-type
	   :enter (do
		    (actor-println "Customer" id "is entering.")
		    (send-msg waiting-room :enter)
		    id)
	   :full (do
		   (actor-println "Customer" id "is leaving.")
		   id)
	   :wait (do
		   (actor-println "Customer" id "is waiting.")
		   id)
	   :start (do
		    (actor-println "Customer" id "is being served.")
		    id)
	   :done (do
		   (actor-println "Customer" id "has been served.")
		   id)
	   id))))

(defn sleeping-barber []
  (send-msg barber :wait)
  (doseq [i (range)]
    (send-msg (customer i) :enter)
    (Thread/sleep (rand-int (* 2 *processing-time*)))))

(sleeping-barber)

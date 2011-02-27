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
   {:waiting nil :barber-asleep? true}
   (fn [{:keys [waiting barber-asleep?] :as state}
	[msg-type & args]
	sender]
     (let [self *agent*]
       (case msg-type
	     :enter (let [customer (first args)]
		      (if (= (count waiting) *capacity*)
			(do
			  (send-msg sender :full)
			  state)
			(if barber-asleep?
			  (do
			    (send-msg self :next)
			    (assoc state
			      :waiting (cons customer waiting)
			      :barber-asleep? false))
			  (do
			    (send-msg sender :wait)
			    (assoc state
			      :waiting (cons customer waiting))))))
	     :next (if (> (count waiting) 0)
		     (do
		       (send-msg barber :enter (last waiting))
		       (assoc state
			 :waiting (butlast waiting)))
		     (do
		       (send-msg barber :wait)
		       (assoc state
			 :barber-asleep? true)))
	     state)))))

(defn customer [id]
  (actor
   id
   (fn [id [msg-type & args] sender]
     (let [self *agent*]
       (case msg-type
	     :enter (do
		      (actor-println "Customer" id "is entering.")
		      (send-msg waiting-room :enter self)
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
	     id)))))

(defn sleeping-barber []
  (send-msg barber :wait)
  (doseq [i (range)]
    (send-msg (customer i) :enter)
    (Thread/sleep (rand-int (* 2 *processing-time*)))))

(sleeping-barber)

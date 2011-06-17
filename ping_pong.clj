(ns ping-pong
  (:use actor))

;; Port of 'Ping Pong' example from 'Scala Actors: A Short Tutorial'
;; http://www.scala-lang.org/node/242

(def ponger
  (actor
   0
   (fn [count [msg-type & _] sender]
     (case msg-type
	   :ping (do
		   (if (zero? (rem count 1000))
		     (actor-println "Ponger: ping" count))
		   (send-msg sender :pong)
		   (inc count))
	   :stop (do
		   (actor-println "Ponger: stop")
		   count)
	   count))))

(def pinger
  (actor
   100000
   (fn [count [msg-type & _] sender]
     (case msg-type
	   :start (do
		    (send-msg ponger :ping)
		    (dec count))
	   :pong (do
		   (if (zero? (rem count 1000))
		     (actor-println "Pinger: pong"))
		   (if (> count 0)
		     (do
		       (send-msg sender :ping)
		       (dec count))
		     (do
		       (actor-println "Pinger: stop")
		       (send-msg sender :stop)
		       count)))
	   count))))

(send-msg pinger :start)

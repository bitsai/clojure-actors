(ns ping-pong
  (:require actor))

;; Port of 'Ping Pong' example from 'Scala Actors: A Short Tutorial'
;; http://www.scala-lang.org/node/242

(def ponger
  (actor/actor
   0
   (fn [msg count sender]
     (let [msg-type (first msg)]
       (case msg-type
         :ping (do (when (zero? (rem count 1000))
                     (actor/println "Ponger: ping" count))
                   (actor/send sender :pong)
                   (inc count))
         :stop (do (actor/println "Ponger: stop")
                   count))))))

(def pinger
  (actor/actor
   100000
   (fn [msg count sender]
     (let [msg-type (first msg)]
       (case msg-type
         :start (do (actor/send ponger :ping)
                    (dec count))
         :pong (do (when (zero? (rem count 1000))
                     (actor/println "Pinger: pong"))
                   (if (> count 0)
                     (do (actor/send sender :ping)
                         (dec count))
                     (do (actor/println "Pinger: stop")
                         (actor/send sender :stop)
                         count))))))))

(actor/send pinger :start)

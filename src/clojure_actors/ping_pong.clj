(ns clojure-actors.ping-pong
  (:require [clojure-actors.core :as actor])
  (:use [clojure.core.match :only [match]]))

(actor/make
 :pinger
 0
 (fn [n msg]
   (match msg
          [:set-n new-n] new-n
          [:pong] (when (pos? n)
                    (actor/send :printer "Pinger: ponged" n)
                    (actor/send :ponger :ping)
                    (dec n)))))

(actor/make
 :ponger
 nil
 (fn [_ msg]
   (match msg
          [:ping] (do (actor/send :printer "Ponger: pinged")
                      (actor/send :pinger :pong)))))

(defn ping-pong [n]
  (actor/send :pinger :set-n n)
  (actor/send :pinger :pong))

(ns clojure-actors.ping-pong
  (:require [clojure-actors.core :as actor])
  (:use [clojure.core.match :only [match]]))

(actor/make
 :pinger
 0
 (fn [cur-n msg]
   (match msg
          [:set-n n] n
          [:pong] (when (pos? cur-n)
                    (actor/send :printer "Pinger ponged" cur-n)
                    (actor/send :ponger :ping)
                    (dec cur-n)))))

(actor/make
 :ponger
 nil
 (fn [_ msg]
   (match msg
          [:ping] (do (actor/send :printer "Ponger pinged")
                      (actor/send :pinger :pong)))))

(defn ping-pong [n]
  (actor/send :pinger :set-n n)
  (actor/send :pinger :pong))

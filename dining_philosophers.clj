(ns dining-philosophers
  (:use actor))

;; Implements the Chandy/Misra solution to 'Dining Philosophers' problem
;; http://en.wikipedia.org/wiki/Dining_philosophers

(def *eating-time* 300)
(def *thinking-time* 300)
(def *waiting-time* 300)

(defn msg-fn [{:keys [id l-neighbor r-neighbor l-fork r-fork] :as state}
	      [msg-type & args]
	      sender]
  (let [self *agent*
	left? (= sender l-neighbor)
	right? (= sender r-neighbor)]
    (case msg-type
	  :add-state (merge state (first args))
	  :hungry (do
		    (if (nil? l-fork)
		      (send-msg l-neighbor :request))
		    (if (nil? r-fork)
		      (send-msg r-neighbor :request))
		    (if (= :clean l-fork r-fork)
		      (do
			(actor-println "philosopher" id "is eating!")
			(Thread/sleep *eating-time*)
			(send-msg self :think)
			(assoc state :l-fork :dirty :r-fork :dirty))
		      (do
			(Thread/sleep *waiting-time*)
			(send-msg self :hungry)
			state)))
	  :think (do
		   (actor-println "philosopher" id "is thinking!")
		   (Thread/sleep *thinking-time*)
		   (send-msg self :hungry)
		   state)
	  :fork (cond
		 (and left? (nil? l-fork)) (assoc state :l-fork :clean)
		 (and right? (nil? r-fork)) (assoc state :r-fork :clean)
		 :else state)
	  :request (cond
		    (and left? (= :dirty l-fork))
		    (do
		      (send-msg l-neighbor :fork)
		      (assoc state :l-fork nil))
		    (and right? (= :dirty r-fork))
		    (do
		      (send-msg r-neighbor :fork)
		      (assoc state :r-fork nil))
		    :else state)
	  state)))

(defn philosophers []
  (for [i (range)]
    (actor {:id i} msg-fn)))

(defn set-neighbors [philosophers]
  (let [n (count philosophers)]
    (doseq [i (range n)]
      (let [pi-1 (nth philosophers (mod (dec i) n))
	    pi (nth philosophers i)
	    pi+1 (nth philosophers (mod (inc i) n))]
	(send-msg pi :add-state {:l-neighbor pi-1 :r-neighbor pi+1})))))

(defn set-forks [[first-philosopher & philosophers]]
  (send-msg first-philosopher :add-state {:l-fork :dirty :r-fork :dirty})
  (doseq [p (butlast philosophers)]
    (send-msg p :add-state {:l-fork nil :r-fork :dirty}))
  (send-msg (last philosophers) :add-state {:l-fork nil :r-fork nil}))

(defn dining-philosophers [n]
  (if (> n 1)
    (let [philosophers (take n (philosophers))]
      (set-neighbors philosophers)
      (set-forks philosophers)
      (doseq [p philosophers]
	(send-msg p :think)))))

(dining-philosophers 5)

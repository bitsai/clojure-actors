(ns dining-philosophers
  (:use actor))

;; Implements the Chandy/Misra solution to 'Dining Philosophers' problem
;; http://en.wikipedia.org/wiki/Dining_philosophers

(defn msg-fn [{:keys [name l-phil r-phil l-fork r-fork] :as state}
	      msg
	      args
	      sender]
  (let [left? (= sender l-phil)
	right? (= sender r-phil)]
    (case msg
	  :set-state (merge state (first args))
	  :hungry (do
		    (if (nil? l-fork)
		      (send-msg l-phil :request))
		    (if (nil? r-fork)
		      (send-msg r-phil :request))
		    (if (= :clean l-fork r-fork)
		      (do
			(actor-println name "is eating!")
			(Thread/sleep 300)
			(send-msg *agent* :think)
			(assoc state :l-fork :dirty :r-fork :dirty))
		      (do
			(Thread/sleep 300)
			(send-msg *agent* :hungry)
			state)))
	  :think (do
		   (actor-println name "is thinking!")
		   (Thread/sleep 300)
		   (send-msg *agent* :hungry)
		   state)
	  :fork (cond
		 (and left? (nil? l-fork)) (assoc state :l-fork :clean)
		 (and right? (nil? r-fork)) (assoc state :r-fork :clean)
		 :else state)
	  :request (cond
		    (and left? (= :dirty l-fork))
		    (do
		      (send-msg l-phil :fork)
		      (assoc state :l-fork nil))
		    (and right? (= :dirty r-fork))
		    (do
		      (send-msg r-phil :fork)
		      (assoc state :r-fork nil))
		    :else state)
	  state)))

(defn philosophers []
  (for [i (range)]
    (actor {:name i} msg-fn)))

(defn set-neighbors [phils]
  (let [n (count phils)]
    (doseq [i (range n)]
      (let [p_i-1 (nth phils (mod (dec i) n))
	    p_i (nth phils i)
	    p_i+1 (nth phils (mod (inc i) n))]
	(send-msg p_i :set-state {:l-phil p_i-1 :r-phil p_i+1})))))

(defn set-forks [[first-phil & phils]]
  (send-msg first-phil :set-state {:l-fork :dirty :r-fork :dirty})
  (doseq [phil (butlast phils)]
    (send-msg phil :set-state {:l-fork nil :r-fork :dirty}))
  (send-msg (last phils) :set-state {:l-fork nil :r-fork nil}))

(defn dining-philosophers [n]
  (if (> n 1)
    (let [phils (take n (philosophers))]
      (set-neighbors phils)
      (set-forks phils)
      (doseq [phil phils]
	(send-msg phil :think)))))

(dining-philosophers 5)

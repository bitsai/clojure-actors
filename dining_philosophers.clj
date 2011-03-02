(ns dining-philosophers
  (:use actor))

;; Implements the Chandy/Misra solution to 'Dining Philosophers' problem
;; http://en.wikipedia.org/wiki/Dining_philosophers

(def *eating-time* 300)
(def *thinking-time* 300)

(defn post-eat-state [{:keys [l-asked? r-asked?] :as state}]
  (cond
   (and l-asked? r-asked?)
   (assoc state :l-fork nil :r-fork nil :l-asked? false :r-asked? false)
   l-asked?
   (assoc state :l-fork nil :r-fork :dirty :l-asked? false)
   r-asked?
   (assoc state :l-fork :dirty :r-fork nil :r-asked? false)
   :else
   (assoc state :l-fork :dirty :r-fork :dirty)))

(defn msg-fn [{:keys [id l-phil r-phil l-fork r-fork l-asked? r-asked?]
	       :as state}
	      [msg-type & args]
	      sender]
  (let [self *agent*]
    (case msg-type
	  :add-state (merge state (first args))
	  :try-to-eat (do
			(if (not= :clean l-fork)
			  (send-msg l-phil :ask :right))
			(if (not= :clean r-fork)
			  (send-msg r-phil :ask :left))
			(if (= :clean l-fork r-fork)
			  (send-msg self :eat))
			state)
	  :eat (do
		 (actor-println "philosopher" id "is eating!")
		 (Thread/sleep *eating-time*)
		 (send-msg self :think)
		 (if l-asked?
		   (send-msg l-phil :fork :right))
		 (if r-asked?
		   (send-msg r-phil :fork :left))
		 (post-eat-state state))
	  :think (do
		   (actor-println "philosopher" id "is thinking!")
		   (Thread/sleep *thinking-time*)
		   (send-msg self :try-to-eat)
		   state)
	  :fork (let [from (first args)]
		  (case from
			:left (do
				(if (= :clean r-fork)
				  (send-msg self :eat))
				(assoc state :l-fork :clean))
			:right (do
				 (if (= :clean l-fork)
				   (send-msg self :eat))
				 (assoc state :r-fork :clean))
			state))
	  :ask (let [from (first args)]
		 (case from
		       :left (if (= :dirty l-fork)
			       (do
				 (send-msg l-phil :fork :right)
				 (assoc state :l-fork nil))
			       (assoc state :l-asked? true))
		       :right (if (= :dirty r-fork)
				(do
				  (send-msg r-phil :fork :left)
				  (assoc state :r-fork nil))
				(assoc state :r-asked? true))
		       state))
	  state)))

(defn philosophers []
  (for [i (range)]
    (actor {:id i :l-asked? false :r-asked? false} msg-fn)))

(defn set-neighbors [philosophers]
  (let [n (count philosophers)]
    (doseq [i (range n)]
      (let [pi-1 (nth philosophers (mod (dec i) n))
	    pi (nth philosophers i)
	    pi+1 (nth philosophers (mod (inc i) n))]
	(send-msg pi :add-state {:l-phil pi-1 :r-phil pi+1})))))

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

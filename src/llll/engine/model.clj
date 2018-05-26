(ns llll.engine.model
  (:require [llll.engine.variables :as v]))

(defn ->Line
  [name                ; line-key
   queue               ; actions to run next osc
   queue-gen-func      ; generator to refill queue
   state               ; object represented line state
   state-update-func ; function to update state when queue is refilled
   ]
  {:name name :queue queue :queue-gen-func queue-gen-func
   :state state :state-update-func state-update-func})

(defn make-line [line-key generator initial-state state-update-func]
  (let [queue (generator initial-state)
        next-state (if state-update-func (state-update-func initial-state) initial-state)]
    (->Line line-key queue generator next-state state-update-func)))

(defn ->Schedule
  [line-key                             ; line-key to be put
   id                                   ; uniq id for schedule
   tick                                 ; time to run action
   func                                 ; 0-arity function
   ]
  {:line-key line-key :id id :tick tick :func func})

(defn ->Queue [dur actions]
  {:dur dur :actions actions})

(defn ->Action [tick func]
  {:tick tick :func func})

(defn event-queue [dur f]
  (->Queue dur [(->Action 0 f)]))

(defn- calcurate-next-line [{:keys [queue queue-gen-func once-queue-modifier
                                    state state-update-func] :as line}]
  (let [next-state (if state-update-func
                     (state-update-func state)
                     state)
        next-queue (if queue-gen-func
                     (if once-queue-modifier
                       (once-queue-modifier queue-gen-func next-state)
                       (queue-gen-func next-state))
                     nil)]
    (-> line
        (assoc :queue next-queue
               :state next-state)
        (dissoc :once-queue-modifier))))

(defn- merge-queue [acc-queue queue]
  (let [tick-offset (:dur acc-queue)]
    (->Queue (+ (:dur acc-queue) (:dur queue))
             (concat (:actions acc-queue)
                     (->> (:actions queue)
                          (map (fn [m] (update m :tick #(+ tick-offset %)))))))))

(defn- shift-tick [{:keys [dur actions]} offset]
  (->Queue (- dur offset)
           (map (fn [m] (update m :tick #(- % offset))) actions)))

(defn- split-queue [{:keys [dur actions]} tick]
  (let [{smaller true bigger false} (group-by #(< (:tick %) tick) actions)]
    [(if smaller (->Queue tick smaller) (->Queue tick []))
     (if bigger (-> (->Queue dur bigger)
                    (shift-tick tick))
         (->Queue (- dur tick) []))]))

(defn- take-while-period [ticks-in-period initial-line]
  (loop [{:keys [queue] :as line} initial-line
         acc-queue (->Queue 0 [])]
    (if (nil? queue)
      {:line line :queue acc-queue}

      (let [acc-queue2 (merge-queue acc-queue queue)]
        (if (<  (:dur acc-queue2) ticks-in-period)
          (recur (calcurate-next-line line) acc-queue2)

          (let [[smaller bigger] (split-queue acc-queue2 ticks-in-period)]
            {:line (-> line
                       (assoc :queue bigger))
             :queue smaller}))))))


(defn- uuid [] (str (java.util.UUID/randomUUID)))
(defn- make-schedule [line-key {:keys [tick func]}]
  (->Schedule line-key (uuid) tick func))

(defn calcurate-next-schedules
  [{line-key :name :as line}]
  (let [ticks-in-period v/*total-tick-per-osc*
        {next-line :line, queues :queue} (take-while-period ticks-in-period line)
        schedules (->> (:actions queues)
                       (map (partial make-schedule line-key)))]
    {:line next-line :schedules schedules}))

(defn- assoc-not-nil [m key v]
  (if (nil? v) m
      (assoc m key v)))

(defn- update-queue [line queue]
  (cond
    (nil? queue) line

    (nil? (:queue line)) (assoc line :queue queue)

    true (-> line
             (update-in  [:queue :actions] #(->> (concat % (:actions queue))
                                                 (sort-by :tick)))
             (update-in [:queue :dur] #(max % (:dur queue))))))

(defn merge-line [x {:keys [queue state] :as y}]
  (-> (reduce-kv assoc-not-nil x (-> y
                                     (dissoc :queue :state)))
      (update :state #(merge state %))
      (update-queue queue)))

(defn put-once-queue-modifier [line modifier]
  (assoc line :once-queue-modifier modifier))

(defn put-once-event [line f]
  (put-once-queue-modifier
   line
   (fn [gen-func state]
     (-> (gen-func state)
         (update :actions #(cons (->Action 0 (fn [] (f state))) %))))))

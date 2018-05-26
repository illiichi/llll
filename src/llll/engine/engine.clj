(ns llll.engine.engine
  (:require [llll.engine.variables :as v]
            [llll.engine.model :as m]
            [overtone.osc :as ot-osc]
            [overtone.at-at :as at]
            [overtone.osc :as ot-client]))

(defonce ^:dynamic *my-pool* (at/mk-pool))

(defonce ^:private server (ot-osc/osc-server 57128))

(defn- register-schedule [base-time {:keys [tick func]}]
  (let [time (* v/*tick-interval* tick)]
    (at/at (+ base-time time) func *my-pool* )))

(defonce %lines (atom {}))

(defn- try-step-line-and-schedule [base-time org-line]
  (try
    (let [{:keys [line schedules]} (m/calcurate-next-schedules org-line)]
      (doseq [schedule schedules]
        (register-schedule base-time schedule))
      line)
    (catch Throwable e
      (do
        (prn "exception in step line:" (:name org-line) e)
        org-line))))

(defn- map-values [f m]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) {} m))

(defn- elminate-empty-line [lines]
  (let [empty-keys (->> lines
                        (filter (fn [[_ {:keys [queue queue-gen-func]}]] (and (nil? queue-gen-func)
                                                                              (nil? queue))))
                        keys
                        )]
    (doseq [key empty-keys] (prn "elminated:" key))
    (apply dissoc lines empty-keys)))

(defn- schedule-for-next-period [base-time]
  (swap! %lines
         (fn [lines]
           (->> lines
                (map-values (partial try-step-line-and-schedule base-time))
                (elminate-empty-line)))))

(defn- osc-listener
  ([{:keys [on-received osc-path]} {path :path args :args}]
   (when (= path osc-path)
     (let [current-time (at/now)
           base-time (+ current-time v/*time-osc-interval*)]
       (when on-received (on-received base-time))
       (overtone.at-at/at (- base-time v/*scheduling-delay*)
                          #(schedule-for-next-period base-time)
                          *my-pool*)))))
(defn start-osc-listener
  ([] (start-osc-listener {}))
  ([options] (ot-osc/osc-listen server (partial osc-listener options) :trigger)))

(defn stop-osc-listener [] (ot-osc/osc-rm-all-listeners server))

(defn merge-with-keys [to from keys]
  (into to (map (fn [k] [k (k from)]) keys)))

(defn register-line [{line-key :name :as new-line}]
  (swap! %lines (fn [lines]
                  (if-let [line (line-key lines)]
                    (update lines line-key #(m/merge-line % new-line))
                    (assoc lines line-key new-line)))))

(defn register-queue [line-key param-key queue]
  (let [temp-line (m/->Line (keyword (str (name line-key) "-" (name param-key)))
                            queue nil nil nil)]
    (register-line temp-line)))

(defn unregister-line [line-key]
  (let [line (line-key @%lines)]
    (if-let [finalizer (:finalizer line)]
      (finalizer (:state line))))
  (swap! %lines (fn [lines] (dissoc lines line-key))))

(defn refresh-lines []
  (doseq [line-key (keys @%lines)]
    (unregister-line line-key)))

(defn refresh-lines-force []
  (reset! %lines {}))

(defn update-line-state [line-key f]
  (swap! %lines (fn [lines]
                  (update lines line-key #(update % :state f)))))

(defn put-finalizer [line f]
  (assoc line :finalizer f))

(defn dump-lines-state []
  (let [table @%lines]
    (if (empty? table)
      (println "empty")

      (doseq [[line-key line] table]
        (println " --- " line-key " --- ")
        (println (dissoc (:state line) :node-holder))))))

(def ^:private server-host (ot-client/osc-client "localhost" 57128))
(defn send-metronome-osc [osc-path]
  (ot-client/osc-send server-host osc-path))

(defonce ^:dynamic ^:private *osc-client-pool* (at/mk-pool))
(defn start-osc-client
  ([options] (start-osc-client options (at/now)))
  ([{:keys [osc-path] :as options} now]
   (let [next-time (+ now v/*time-osc-interval*)]
     (send-metronome-osc osc-path)
     (at/at next-time #(start-osc-client options next-time) *osc-client-pool*))))

(defn stop-osc-client []
  (at/stop-and-reset-pool! *osc-client-pool* :strategy :kill))

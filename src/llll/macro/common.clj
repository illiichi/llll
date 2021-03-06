(ns llll.macro.common
  (:require [llll.engine.model :as m]
            [llll.engine.variables :as v]
            [llll.engine.engine :as en]
            [llll.sc.groups :as g]
            [llll.sc.sound-control :as ctl]
            [llll.sc.node-switch :as sw]))

(def ^:private default-swap-option
  {:out-bus 0
   :fade-in-dur 0
   :fade-out-dur 0
   :switch-dur 0})

(defn- create-initial-state-exp [{:keys [initial]}]
  `(-> ~initial
       (assoc :node-holder (sw/->Holder))))

(defn- create-swap-option-exp [swap-option period]
  `(-> (merge ~default-swap-option ~swap-option)
       (assoc :dur (* ~period v/*tick-interval-sec*))
       (update :switch-dur #(* % v/*tick-interval-sec*))
       (update :fade-in-dur #(* % v/*tick-interval-sec*))
       (update :fade-out-dur #(* % v/*tick-interval-sec*))))

(defn- make-keep-node-holder-exp [func-exp]
  (if func-exp
    `(fn [{:keys [~'node-holder] :as state#}]
       (-> (~func-exp state#)
           (assoc :node-holder ~'node-holder)))))

(defn- create-queue-gen-exp [arg-names body]
  `(fn [{:keys ~(conj arg-names 'node-holder)}] ~body))

(defn- create-group-and-out-bus-exp [line-key layer-no group-position rate]
  (let [create-bus (if (= :control rate) `ctl/bus `ctl/sound-bus)]
    (cond
      layer-no
      `{:group (ctl/common-group ~layer-no)
        :out-bus (if (= ~layer-no 0) 0 (~create-bus ~line-key :out-bus))}

      group-position
      `{:group (ctl/group ~line-key ~group-position)
        :out-bus (~create-bus ~line-key :out-bus)}

      true
      `{:group (ctl/group ~line-key nil) ; nil の場合 g/long-lifeになる
        :out-bus (~create-bus ~line-key :out-bus 0)})))

(defn create-register-line-exp
  [name {:keys [rate layer-no group-position period state swap-option] :as options}
   state-keys
   queue-gen-body extra-exps]
  (let [line-key (keyword name)
        initial-state (create-initial-state-exp state)
        swap-option (create-swap-option-exp swap-option period)
        ]
    `(let [~'--swap-option-- (merge ~swap-option
                                    ~(create-group-and-out-bus-exp
                                      line-key layer-no group-position rate))]
       (en/register-line
        (-> (m/->Line ~line-key (m/->Queue 0 [])
                      ~(create-queue-gen-exp state-keys queue-gen-body)
                      ~initial-state ~(make-keep-node-holder-exp (:update state)))
            (en/put-finalizer (fn [{:keys [~'node-holder]}]
                                (sw/finish ~'--swap-option-- ~'node-holder)))
            ~@extra-exps)))))

(defn common-options [options]
  [(-> options :state :initial eval keys vec)
   (merge {:period 8} options)])

(defn create-initial-node-creation-exp [state-keys synth args switch?]
  `(m/put-once-event
    (fn [{:keys ~(conj state-keys 'node-holder)}]
      (~(if switch? sw/switch sw/launch) ~'--swap-option-- ~'node-holder ~synth ~args))))

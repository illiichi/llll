(ns llll.macro.defsound
  (:require [llll.engine.engine :as en]
            [llll.engine.model :as m]
            [llll.sc.sound-control :as snd]
            [llll.engine.variables :as v]
            [llll.sc.synth :as syn]
            [llll.sc.node-switch :as sw]
            [llll.macro.split-synth-body :as sp]
            [llll.macro.common :as common]))

(def ^:private default-synth-option {:type :normal})

(defn- create-output-synth-exp [synth-option name args body]
  (let [synth-option (merge default-synth-option synth-option)]
    `(syn/define-output-synth ~name ~args ~synth-option ~body)))

(defn- make-synth-name [base-name number]
  (symbol (str base-name "-" number)))

(defn- create-queue-gen-body-exp-for-keep-node [{:keys [period]} {:keys [arg-names arg-values]}]
  (if (empty? arg-names)
    `(m/->Queue ~period [])

    `(m/event-queue ~period
                    (fn []
                      (sw/send-args ~'node-holder ~(vec (interleave arg-names arg-values)))))))

(defn- make-synth-table [name synths]
  (->> synths
       (map :number)
       (map (fn [n] [n (make-synth-name name n)]))
       (into {})))

(defn- create-queue-gen-body-exp-for-sections [name {:keys [period]} func-exp synth-table-exp]
  `(let [synth-table# ~synth-table-exp]
     (sp/convert->Queue ~period
                        (fn [number# dur# args#]
                          #(sw/switch (-> ~'--swap-option--
                                          (assoc :dur (* dur# v/*tick-interval-sec*)))
                                     ~'node-holder (synth-table# number#) args#))
                        ~func-exp)))

(defn- create-initial-node-creation-exp [name {:keys [arg-names arg-values]} state-keys]
  (common/create-initial-node-creation-exp
   state-keys
   (make-synth-name name 1)
   (vec (interleave arg-names arg-values))
   true))

(defmacro defsound [name options body]
  (let [[state-keys options] (common/common-options (eval options))
        {:keys [keep-node? func-exp synths]} (sp/split-synth-body (keyword name) body)
        synth-option (assoc (:synth-option options) :vol-bus `(snd/bus ~(keyword name) :vol))
        synths-exp (->> synths
                        (map #(assoc % :synth-name (make-synth-name name (:number %))))
                        (map (fn [{:keys [synth-name args-def exp]}]
                               (create-output-synth-exp synth-option synth-name args-def exp))))
        queue-gen-body (if keep-node?
                         (create-queue-gen-body-exp-for-keep-node options (first func-exp))
                         (create-queue-gen-body-exp-for-sections name options func-exp
                                                                 (make-synth-table name synths)))
        extra-exps (if keep-node?
                     [(create-initial-node-creation-exp name (first func-exp) state-keys)]
                     [])
        register-line-exp (common/create-register-line-exp name options state-keys queue-gen-body extra-exps)]
    `(do ~@synths-exp
         ~register-line-exp)))



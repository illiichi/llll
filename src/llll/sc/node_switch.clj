(ns llll.sc.node-switch
  (:require [overtone.sc.node :as ot-n]))

(defn ->Holder
  ([] (atom {})))

(defn- make-node [{:keys [group dur out-bus]} fade-in-dur synth args]
  (let [args-array (concat [:dur dur
                            :out-bus out-bus :arg-vol 1
                            :fade-in-dur fade-in-dur]
                           args)]
    (apply synth [:tail group] args-array)))

(defn- start-node [{:keys [fade-in-dur] :as switch-option} synth args]
  (let [switch-option (assoc switch-option :fade-in-dur 0)]
    (make-node switch-option fade-in-dur synth args)))

(defn- switch-node [{:keys [switch-dur switch-fade-in switch-fade-out] :as switch-option}
                    old-node synth args]
  (ot-n/ctl old-node :fade-out-dur (or switch-fade-out switch-dur) :fade-out-gate 0)
  (make-node switch-option (or switch-fade-in switch-dur) synth args))

(defn- switch-or-launch [launch? switch-option holder synth args]
  (locking holder
    (let [node     (:node @holder)
          new-node (if (and node (ot-n/node-active? node))
                     (if launch? node (switch-node switch-option node synth args))
                     (start-node switch-option synth args))]
      (reset! holder {:node new-node}))))

(defn launch [switch-option holder synth args]
  (switch-or-launch true switch-option holder synth args))

(defn switch [switch-option holder synth args]
  (switch-or-launch false switch-option holder synth args))

(defn send-args [holder args]
  (let [node     (:node @holder)]
    (apply ot-n/ctl node args)))

(defn finish [{:keys [fade-out-dur]} holder]
  (let [node     (:node @holder)]
    (when (ot-n/node-active? node)
      (ot-n/ctl node :fade-out-dur fade-out-dur :fade-out-gate 0))
    (reset! holder {})))

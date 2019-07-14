(ns llll.sc.sound-control
  (:require [overtone.sc.bus :as ot-bus]
            [overtone.sc.synth :as ot-synth]
            [overtone.sc.node :as ot-node]
            [overtone.sc.ugens :as ot-u]
            [overtone.sc.cgens.env :as ot-env]
            [llll.engine.variables :as v]
            [llll.sc.groups :as g]
            [llll.sc.synth :as synth]))

(defonce %buses (atom {}))
(defonce %sound-buses (atom {}))
(defonce %groups (atom {}))

(defn- get-or-put-new [atom-m key create-func]
  (or (key (deref atom-m))
      (let [x (create-func)]
        (swap! atom-m #(assoc % key x))
        x)))

(defn- put-force [atom-m key v]
  (swap! atom-m #(assoc % key v))
  v)

(defn bus
  [line-key param-name]
  (let [bus-key (keyword (str (name line-key) "-" (name param-name)))]
    (get-or-put-new %buses bus-key ot-bus/control-bus)))

(defn sound-bus
  ([line-key] (sound-bus line-key :self)) ; for defpattern
  ([line-key param-name]
   (let [bus-key (keyword (name line-key))]
     (get-or-put-new %sound-buses bus-key #(ot-bus/audio-bus 2))))
  ([line-key param-name v]
   (let [bus-key (keyword (name line-key))]
     (put-force %sound-buses bus-key v))))

(defn group
  [line-key {:keys [position target] :as option}]
  (let [group-key (keyword (name line-key))]
    (if option
      (let [new-group-func (if target
                             (fn [] (ot-node/group (name line-key) position
                                             (->> target name keyword (get @%groups))))
                             (fn [] (ot-node/group (name line-key))))]
        (get-or-put-new %groups group-key new-group-func))
      g/long-life)))

(defn common-group [no]
  (group (keyword (str "common-" no)) (if (= no 0) {}
                                          {:position :before
                                           :target (str "common-" (dec no))})))

(defn create-common-group [max]
  (doseq [no (range (inc max))]
    (common-group no)))

(declare send-pulse)
(declare line-to-bus)
(declare x-line-to-bus)
(declare out-sound-with-vol-control)
(defonce %gate-bus (ot-bus/control-bus))

(defn initialize []
  (ot-synth/defsynth send-pulse-synth []
    (ot-u/out:kr %gate-bus (ot-env/hold (ot-u/impulse (/ 1000 v/*tick-interval*))
                               (/ (* v/*tick-interval* v/*total-tick-per-osc*) 1000)
                               0
                               ot-u/FREE)))

  (ot-synth/defsynth line-to-bus [bus 0
                                  start 0
                                  end 0
                                  dur 0]
    (ot-u/out:kr bus (ot-u/line:kr start end dur ot-u/FREE)))

  (ot-synth/defsynth x-line-to-bus [bus 0
                                    start 0
                                    end 0
                                    dur 0]
    (ot-u/out:kr bus (ot-u/x-line:kr start end dur ot-u/FREE)))

  (def out-sound-with-vol-control
    (synth/define-output-synth output-synth [input-bus -1 vol-bus -1]
      {:type :no-decoration}
      (let [input-bus (ot-u/in:ar input-bus 2)
            vol-bus (ot-u/in:kr vol-bus)]
        (ot-u/tanh (* input-bus vol-bus))))))

(def ^:dynamic *show-metronome* false)
(defn send-global-pulse [current-time]
  (when *show-metronome* (println "metronome:" current-time))
  (send-pulse-synth))

(defn make-out-node
  ([sound-bus vol-bus]
   (make-out-node sound-bus vol-bus out-sound-with-vol-control))
  ([sound-bus vol-bus output-synth]
   (output-synth [:tail g/long-life] :out-bus 0 :input-bus sound-bus :vol-bus vol-bus)))

(def ^:const dump-threthold 1e-8)

(defn dump-control-values []
  (let [table @%buses]
    (if (empty? table)
      (println "empty")

      (doseq [[bus-key b] table]
        (let [v (first (ot-bus/control-bus-get b))]
          (when (> v dump-threthold) (println bus-key ":" v)))))))

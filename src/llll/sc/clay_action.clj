(ns llll.sc.clay-action
  (:require [llll.engine.model :as m]
            [llll.sc.sound-control :as snd]
            [llll.sc.groups :as g]
            [overtone.sc.bus :as ot-bus]))

(defn make-func-for-play-sound
  [synth sound-bus]
  (fn [dur-in-sec args]
    (fn [] (let [args (->> (assoc args :out-bus sound-bus)
                           (mapcat identity))]
             (apply synth [:tail g/short-life] dur-in-sec args)))))

(def ^:private control-sample-length* (/ 1 689))


(defn- set-param-func [bus v]
  (fn [] (ot-bus/control-bus-set! bus v)))

(defn- control-param-func
  ([bus dur-in-sec changing-func dest-func]
   (fn [] (let [current-value (first (ot-bus/control-bus-get bus))
                dest-value (dest-func current-value)]
            (changing-func bus current-value dest-value (- dur-in-sec control-sample-length*)))))

  ([bus dur-in-sec changing-func start end]
   (fn [] (changing-func bus start end (- dur-in-sec control-sample-length*)))))

(defn make-func-for-control-param
  [line-key param-key]
  (let [b (snd/bus line-key param-key)]
    (fn [dur-in-sec {:keys [set relative? linear? v v-prev] :as args}]
      (if set (set-param-func b set)
          (let [changing-func (if linear? snd/line-to-bus snd/x-line-to-bus)
                dest-func (if relative? #(+ v %) (fn [_] v)) ]
            (if v-prev (control-param-func b dur-in-sec changing-func v-prev (dest-func v-prev))
                (control-param-func b dur-in-sec changing-func dest-func)))))))



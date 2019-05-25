(ns llll.macro.control
  (:require [llll.engine.engine :as en]
            [llll.clay.clay :as c]
            [llll.sc.clay-action :as a]
            [llll.sc.sound-control :as snd]
            [llll.clay.transition-dsl :as tr]
            [overtone.sc.bus :as ot-bus]))

(defn control [line-key param-key {:keys [dur from to set]}]
  (assert (or (not (nil? set))
              (and (not (nil? dur))
                   (not (nil? to)))))
  (if set
    (ot-bus/control-bus-set! (snd/bus line-key param-key) set)

    (let [param (if from
                  {:linear? true :v-prev from :v to}
                  {:linear? true :v to})]
      (en/register-queue
       line-key param-key
       (c/burn {:symbol-table {:x (a/make-func-for-control-param line-key param-key)}}
               dur
               (c/create-clay :x param))))))


(defmacro --> [clay & transitions]
  `(tr/assign-transition ~(tr/parse transitions) ~clay))


(ns llll.sc.synth
  (:require [overtone.sc.synth :as ot-synth]
            [overtone.sc.ugens :as ot-u]
            [overtone.sc.envelope :as ot-env]
            [llll.engine.variables :as v]
            [clojure.walk :as w]))

(declare ^:private special-symbol-table*)

(defn initialize []
  (def ^:private special-symbol-table*
    (-> {:-gate `(~'* ~'fade-out-gate (ot-u/in:kr llll.sc.sound-control/%gate-bus))
         :=gate `(ot-u/in:kr llll.sc.sound-control/%gate-bus)}
        (into (map (fn [i] [(keyword (str "t" i)) (* i v/*tick-interval-sec*)])
                   (range 1 19)))
        (into (map (fn [i] [(keyword (str "f" i)) (/ 1 i v/*tick-interval-sec*)])
                   (range 1 19))))))

(defn- replace-special-symbol-for-synth [synth-exp]
  (w/prewalk-replace special-symbol-table* synth-exp))

(defn- create-volume-wrapped-synth [exp]
  `(~'* ~'arg-vol ~exp))

(defn- create-base-synth-exp [synth-name args-list exp ]
  (let [args-list (vec (concat ['dur 0 'out-bus 0 'arg-vol 1] args-list))
        synth-exp (create-volume-wrapped-synth exp)]
    `(ot-synth/defsynth ~synth-name ~args-list
       (ot-u/out ~'out-bus ~synth-exp))))

(defn create-detune-synth-exp [amount body]
  `(map (fn [~'dr] ~body) [(- 1 ~amount) (+ 1 ~amount)]))

(defn enclose-vol-bus [body vol-bus-exp]
  `(~'* (ot-u/in:kr ~vol-bus-exp) ~body))

(defmacro define-common-synth
  ([synth-name args body] `(define-common-synth ~synth-name ~args {:type :normal} ~body))
  ([synth-name args {:keys [type vol-bus] :as option} body]
   (let [body (replace-special-symbol-for-synth body)
         body (case type
                :normal body
                :detune (create-detune-synth-exp (or (:amount option) 0.01) body))
         body (if vol-bus (enclose-vol-bus body vol-bus) body)
         ]
     (create-base-synth-exp synth-name args body))))


(defn create-fade-inout-synth-exp [body]
  `(~'* (ot-u/env-gen (ot-env/envelope [0 1] [~'fade-in-dur] :linear))
   (ot-u/env-gen (ot-env/envelope [1 1 0] [0 ~'fade-out-dur] :linear 1)
               ~'fade-out-gate :action ot-u/FREE)
   ~body))

(defmacro define-output-synth
  ([synth-name arg-names body] `(define-output-synth ~synth-name ~arg-names {:type :normal} ~body))
  ([synth-name args {:keys [type] :as option} body]
   (let [args (-> (concat ['fade-out-gate 1
                           'fade-in-dur 0
                           'fade-out-dur 0]
                          args)
                  vec)
         body (-> body
                  (create-fade-inout-synth-exp))]
     (if (= type :no-decoration)
       (create-base-synth-exp synth-name args body)
       `(define-common-synth ~synth-name ~args ~option ~body)))))

(ns llll.macro.defpattern
  (require [llll.clay.conversions :as conv]
           [llll.clay.clay :as c]
           [llll.sc.sound-control :as snd]
           [llll.engine.model :as m]
           [llll.sc.clay-action :as action]
           [llll.macro.common :as common]))

(intern *ns* (with-meta 'defsynth-l4 {:macro true}) @#'llll.sc.synth/define-common-synth)
(intern *ns* (with-meta '| {:macro true}) @#'llll.clay.conversions/parse)
(intern *ns* '=| @#'llll.clay.clay/assign)
(intern *ns* '&| @#'llll.clay.clay/merge-clay)

(defn- map-values [f m]
  (reduce-kv (fn [acc k v] (into acc {k (f v)})) {} m))

(defmulti convert-to-clay-action first)
(defmethod convert-to-clay-action 'synth [[_ sym]]
  `(action/make-func-for-play-sound ~sym ~'--sound-bus--))

(defn- create-symbol-table-exp [line-key table]
  `(let [~'--sound-bus-- (snd/sound-bus ~line-key)]
     ~(map-values convert-to-clay-action table)))

(defn- create-queue-gen-body [{:keys [period ]} clay]
  `(c/burn ~'--context-- ~period ~clay))

(defn- create-initial-node-creation-exp [line-key state-keys]
  (common/create-initial-node-creation-exp
   state-keys snd/out-sound-with-vol-control
   [:input-bus `(snd/sound-bus ~line-key) :vol-bus `(snd/bus ~line-key :vol)]
   false))

(defmacro defpattern [name options clay]
  (let [line-key (keyword name)
        [state-keys options] (common/common-options options)
        symbol-table (create-symbol-table-exp line-key (:table options))
        queue-gen-body (create-queue-gen-body options clay)
        extra-exp (create-initial-node-creation-exp line-key state-keys)]
    `(let [~'--context-- {:symbol-table ~symbol-table}]
       ~(common/create-register-line-exp name options state-keys queue-gen-body [extra-exp]))))

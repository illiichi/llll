(ns llll.core
  (require [llll.engine.engine :as en]
           [llll.sc.groups :as g]
           [llll.engine.variables :as v]
           [llll.sc.sound-control :as snd]
           [llll.sc.synth :as syn]
           [llll.macro.control]
           [mount.core :as mount]))

(defn initialize [{:keys [bpm beat-per-osc ticks-per-beat osc-client osc-path]
                   :or {bpm 120
                        beat-per-osc 4
                        ticks-per-beat 4
                        osc-client true
                        osc-path "/metronome"}}]
  (v/initialize bpm beat-per-osc ticks-per-beat)
  (syn/initialize)
  (snd/initialize)
  (mount/start)
  (en/start-osc-listener {:osc-path osc-path
                          :on-received snd/send-global-pulse})
  (when osc-client (en/start-osc-client {:osc-path osc-path})))

(defn finish []
  (en/stop-osc-client)
  (en/stop-osc-listener)
  (en/refresh-lines)
  (mount/stop))

(intern *ns* 'control #'llll.macro.control/control)
(intern *ns* 'stop #'llll.engine.engine/unregister-line)
(intern *ns* 'stop-force #'llll.engine.engine/refresh-lines-force)
(intern *ns* 'dump #'llll.engine.engine/dump-lines-state)
(intern *ns* 'dump-bus #'llll.sc.sound-control/dump-control-values)

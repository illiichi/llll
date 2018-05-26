(ns llll.engine.variables)

(declare ^:dynamic *bpm*)
(declare ^:dynamic *beats-per-osc*)
(declare ^:dynamic *ticks-per-beat*)

(declare ^:dynamic *beat-interval*)
(declare ^:dynamic *tick-interval-sec*)
(declare ^:dynamic *tick-interval*)
(declare ^:dynamic *total-tick-per-osc*)
(declare ^:dynamic *time-osc-interval*)
(declare ^:dynamic *scheduling-delay*)

(defn initialize [bpm beats-per-osc ticks-per-beat]
  (def ^:dynamic *bpm* bpm)
  (def ^:dynamic *beats-per-osc* beats-per-osc)
  (def ^:dynamic *ticks-per-beat* ticks-per-beat)
  (def ^:dynamic *beat-interval* (/ (* 60 1000) bpm))
  (def ^:dynamic *tick-interval-sec* (/ 60 *bpm* *ticks-per-beat*))
  (def ^:dynamic *tick-interval* (* *tick-interval-sec* 1000))
  (def ^:dynamic *total-tick-per-osc* (* *ticks-per-beat* *beats-per-osc*))
  (def ^:dynamic *time-osc-interval* (* *beat-interval* *beats-per-osc*))
  (def ^:dynamic *scheduling-delay* (* 1/2 *tick-interval*)))




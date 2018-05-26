(ns llll.engine.engine-test
  (:require [clojure.test :refer :all]
            [llll.engine.variables :as v]
            [llll.engine.engine :as en]
            [llll.engine.model :as m]))

(comment
  (en/start-osc-listener)
  (en/stop-osc-listener)

  (v/initialize 120 4 4)
  (let [initial-queue (m/->Queue 2 [(m/->Action 1 #(prn "hoge"))])
        generator (fn [state] (m/->Queue state (if (= (mod state 2) 1)
                                                 [(m/->Action (dec state) #(prn "fuga" state))]
                                                 [])))]
    (en/register-queue :fuga :vol initial-queue)

    (en/register-line (m/->Line :hoge initial-queue generator 10 inc))
    )

  (en/unregister-line :hoge)
  (en/refresh-lines))

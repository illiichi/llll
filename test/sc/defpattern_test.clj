(ns sc.defpattern-test
  (:require [llll.core :as l4]
            [llll.macro.defpattern :refer :all]
            [llll.macro.control :refer :all]))

(use 'overtone.core)
(connect-external-server "localhost" 57110)
(kill-server)
(l4/finish)

(l4/initialize {})

(l4/control :test :vol {:dur 16 :to 1})

(defsynth-l4 ping [freq 200]
  (* (sin-osc freq)
     (env-gen (env-perc 0.05 dur) :action FREE)))

(defpattern test
  {:table {:a (synth ping)}
   :period 64}
  (->> (| :a*8)
       (=| :freq [440 880 1760])))

(defpattern test2
  {:table {:a (synth ping)}
   :period 32}
  (+| (->> (| :a*4) (=| :freq 2000))
      (->> (| :a*2) (=| :freq 3000))
      (->> (| :a*8) (=| :freq 8000))))


(l4/control :test2 :vol {:dur 16 :to 0.1})



(l4/control :test :vol {:dur 8 :to 1})
(l4/stop :test)

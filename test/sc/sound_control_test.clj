(ns llll.sc.sound-control-test
  (:require [clojure.test :refer :all]
            [llll.core :as l4]
            [llll.engine.variables :as v]
            [llll.engine.model :as m]
            [llll.engine.engine :as en]
            [llll.sc.clay-action :as a]
            [llll.sc.groups :as g]
            [llll.sc.sound-control :as snd]
            [llll.sc.synth :as syn]
            [llll.clay.clay :as c]
            [llll.clay.conversions :as conv])
  (:use [overtone.core]))

(comment
  (connect-external-server "localhost" 57110)
  (kill-server)

  (l4/initialize {})
  (l4/finish)
  
  (v/initialize 120 4 4)
  (syn/initialize)
  (snd/initialize)
  
  (en/start-osc-listener)
  (en/stop-osc-listener)

  )

(comment
  (syn/define-common-synth hoge
    [freq 440]
    (splay (map (fn [i] (* 8 (sin-osc (* i freq))
                           (env-gen (env-perc 0.05 0.5) (impulse 1 (/ i dur)))))
                (range 1 8))))

  (def out-n (snd/make-out-node (snd/sound-bus :hoge) (snd/bus :hoge :vol)))
  
  (def n (hoge [:tail g/short-life] :dur 4 :out-bus (snd/sound-bus :hoge)))

  (control-bus-set! (snd/bus :hoge :vol) 1)
  (control-bus-get (snd/bus :hoge :vol))
  (ctl n :freq 214 )

  (snd/line-to-bus (snd/bus :hoge :vol) 1e-8 1 10)
  (ctl out-n :fade-out-dur 1 :fade-out-gate 0))

(comment
  (syn/define-output-synth hoge-fade
    [freq 800]
    (splay (map (fn [i] (* 8 (sin-osc (* i freq))
                           (env-gen (env-perc 0.05 0.5) (impulse 1 (/ i dur)))))
                (range 1 8))))

  (def n (hoge-fade [:tail g/long-life] 4 0 1 :fade-in-dur 4 :freq (snd/bus :hoge-fade :freq)))

  (control-bus-set! (snd/bus :hoge-fade :vol) 1)
  (control-bus-set! (snd/bus :hoge-fade :freq) 220)

  (ctl n :fade-out-dur 4 :fade-out-gate 0))


(comment
  (syn/define-common-synth hoge2
    [freq 440]
    {:type :detune,
     :amount 0.1}
    (sin-osc (* dr freq)))

  (def n2 (hoge2 [:tail g/short-life] 4 (snd/sound-bus :hoge) 1))
  (snd/make-out-node (snd/sound-bus :hoge) (snd/bus :hoge :vol))
  (let [generator (fn [state]
                    (m/->Queue 16 (if (= (mod state 2) 1)
                                    [(m/->Action 0 #(snd/line-to-bus (snd/bus :hoge :vol) 0 1 (* v/*tick-interval-sec* (- 8 1/128))))
                                     (m/->Action 8 #(snd/line-to-bus (snd/bus :hoge :vol) 1 0.5 (* v/*tick-interval-sec* 6)))
                                     (m/->Action 14 #(snd/line-to-bus (snd/bus :hoge :vol) 0.5 0 (* v/*tick-interval-sec* 1.9)))
                                     ]
                                    [(m/->Action 4 #(control-bus-set! (snd/bus :hoge :vol) 0.5))
                                     (m/->Action 8 #(control-bus-set! (snd/bus :hoge :vol) 0))])))]
    (en/register-line (m/make-line :hoge  generator 10 inc))))




(comment
  (en/unregister-line :hoge)
  (en/refresh-lines))


(comment
  (syn/define-common-synth drum [freq 1000]
    (* (rlpf (white-noise) freq 0.3)
       (env-gen (env-perc 0.05 dur) :action FREE)))
  (syn/define-common-synth ping [freq 1000 attack 0.05]
    (* (sin-osc freq)
       (env-gen (env-perc attack dur) :action FREE)))

  (def context {:symbol-table {:vol (a/make-func-for-control-param :drum :vol)
                               :a (a/make-func-for-play-sound drum (snd/sound-bus :drum))
                               :b (a/make-func-for-play-sound ping (snd/sound-bus :drum))}})
  (control-bus-get (snd/bus :drum :vol))

  (en/register-queue :drum :vol
                     (c/burn context 1 (c/create-clay :vol {:set 0.3})))

  (snd/make-out-node (snd/sound-bus :drum) (snd/bus :drum :vol))

  (en/register-queue
   :drum :sound
   (c/burn context 10
           (c/merge-clay
            (c/concat-clay (c/create-clay :a {:arg-vol 1})
                           (c/create-clay :a {:arg-vol 0.5 :freq 3000}))
            (c/concat-clay (c/create-clay :b {:arg-vol 0.1})
                           (c/create-clay :b {:arg-vol 0.5 :freq 440})
                           (c/create-clay :b {:arg-vol 1 :freq 4000 :attack 0.5})))))
  (en/register-queue
   :hoge :vol
   (c/burn context 80
           (c/concat-clay (c/create-clay :vol {:linear? true :v 1} )
                          (c/create-clay :vol {:linear? true :v-prev 2 :v 1} )
                          (c/create-clay :vol {:linear? true :v 0}))))

  (control-bus-get (snd/bus :drum :vol)))

(comment
  (syn/define-common-synth drum [freq 1000]
    (* (rlpf (white-noise) freq 0.3)
       (env-gen (env-perc 0.05 dur) :action FREE)))
  (syn/define-common-synth ping [freq 1000 attack 0.05]
    (* (sin-osc freq)
       (env-gen (env-perc attack dur) :action FREE)))

  (def context {:symbol-table {:vol (a/make-func-for-control-param :drum :vol)
                               :a (a/make-func-for-play-sound drum (snd/sound-bus :drum))
                               :b (a/make-func-for-play-sound ping (snd/sound-bus :drum))}})
  (control-bus-get (snd/bus :drum :vol))

  (en/register-queue :drum :vol
                     (c/burn context 1 (c/create-clay :vol {:set 0.3})))

  (snd/make-out-node (snd/sound-bus :drum) (snd/bus :drum :vol))

  (en/register-queue
   :drum :sound
   (c/burn context 10
           (c/merge-clay
            (c/concat-clay (c/create-clay :a {:arg-vol 1})
                           (c/create-clay :a {:arg-vol 0.5 :freq 3000}))
            (c/concat-clay (c/create-clay :b {:arg-vol 0.1})
                           (c/create-clay :b {:arg-vol 0.5 :freq 440})
                           (c/create-clay :b {:arg-vol 1 :freq 4000 :attack 0.5})))))

  (en/register-queue
   :drum :sound
   (c/burn context 10
           ))
  (en/register-queue
   :hoge :vol
   (c/burn context 80
           (c/concat-clay (c/create-clay :vol {:linear? true :v 1} )
                          (c/create-clay :vol {:linear? true :v-prev 2 :v 1} )
                          (c/create-clay :vol {:linear? true :v 0}))))

  (control-bus-get (snd/bus :drum :vol))

  (en/register-queue
   :hoge :sound
   (c/burn context 20
           (->> (conv/parse [:a|2*8|12 :b] *2 :a*3 :b*2|8)
                (c/assign {:cycle? true} :freq [1000 500])))))






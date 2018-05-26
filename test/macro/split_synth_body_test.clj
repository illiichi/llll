(ns llll.split-synth-body-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [llll.macro.split-synth-body :as sp]
            [llll.sc.sound-control :as snd]
            [overtone.sc.ugens :as ot-u]))

(def ^:private extract-argments #'llll.macro.split-synth-body/extract-argments)
(def ^:private extraction-client-eval
  (deref #'llll.macro.split-synth-body/extraction-client-eval))
(def ^:private extraction-as-control-bus #'llll.macro.split-synth-body/extraction-as-control-bus)

(deftest extract-arguments-test
  (let [extract-argments (partial extract-argments [extraction-client-eval])]
    (testing "no client code"
      (is (= (extract-argments '(* (sin-osc [440 880]) (+ 1 2)))
             {:exp '(* (sin-osc [440 880]) (+ 1 2)),
              :arguments {}})))
    (testing "extract single"
      (is (= (extract-argments '(* (sin-osc [440 880]) (!! (+ 1 2))))
             {:exp '(* (sin-osc [440 880]) arg-0),
              :arguments {:arg-0 '(+ 1 2)}})))

    (testing "extract two"
      (is (= (extract-argments '(* (sin-osc [440 (!! (if u3 880 440))]) (!! (+ 1 (* 3 2)))))
             {:exp '(* (sin-osc [440 arg-0]) arg-1)
              :arguments {:arg-1 '(+ 1 (* 3 2))
                          :arg-0 '(if u3 880 440)}})))))

(deftest extract-bus-test
  (let [extract-argments (partial extract-argments [(extraction-as-control-bus :hoge)])]
    (testing "no bus"
      (is (= (extract-argments '(* (sin-osc [440 880]) (+ 1 2)))
             {:exp '(* (sin-osc [440 880]) (+ 1 2)),
              :control-bus {}})))

    (testing "one bus"
      (is (= (extract-argments '(* (sin-osc [:-freq 440]) (+ 1 2)))
             {:exp '(* (sin-osc [(overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-freq))
                                 440]) (+ 1 2)),
              :control-bus {:-freq :-freq}})))
    (testing "three bus"
      (is (= (extract-argments '(* (sin-osc [:-freq 440] :-vol) (+ 1 :-hoge)))
             {:exp '(* (sin-osc [(overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-freq))
                                 440]
                                (overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-vol)))
                       (+ 1 (overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-hoge)))),
              :control-bus {:-freq :-freq, :-vol :-vol, :-hoge :-hoge}})))))

(deftest split-synth-body-test
  (testing "no section"
    (is (= (sp/split-synth-body :hoge '(sin-osc (* dr (!! (if u3 440 880)))))

           '{:keep-node? true,
             :func-exp [{:arguments {:arg-0 (if u3 440 880)}, :arg-names [:arg-0], :arg-values [(if u3 440 880)], :dur 1, :number 1}],
             :synths [{:args-def [arg-0 0], :exp (sin-osc (* dr arg-0)), :number 1}]}))
    )
  (testing "split section"
    (is (= (sp/split-synth-body :hoge '[(section (sin-osc (* dr (!! (if u3 440 880)))))
                               (section {:dur 7} (sin-osc (* dr (!! 3) (!! 2) (!! 4))))])

           '{:keep-node? false,
             :func-exp [{:arguments {:arg-0 (if u3 440 880)}, :arg-names [:arg-0], :arg-values [(if u3 440 880)], :dur 1, :number 1}
                        {:arguments {:arg-1 2, :arg-0 3, :arg-2 4}, :arg-names [:arg-0 :arg-1 :arg-2], :arg-values [3 2 4], :dur 7, :number 2}],
             :synths [{:args-def [arg-0 0], :exp (sin-osc (* dr arg-0)), :number 1}
                      {:args-def [arg-0 0 arg-1 0 arg-2 0], :exp (sin-osc (* dr arg-0 arg-1 arg-2)), :number 2}]}

           )))

  (testing "split sexp section"
    (is (= (sp/split-synth-body :hoge '(let [freq 440]
                                [(section {:dur 2} (sin-osc (* dr (!! (if u3 freq (* 2 freq))))))
                                 (section (sin-osc (* dr 440)))]))

           '{:keep-node? false,
             :func-exp (let [freq 440]
                         [{:arguments {:arg-0 (if u3 freq (* 2 freq))}, :arg-names [:arg-0], :arg-values [(if u3 freq (* 2 freq))], :dur 2, :number 1}
                          {:arguments {}, :arg-names [], :arg-values [], :dur 1, :number 2}]),
             :synths [{:args-def [arg-0 0], :exp (sin-osc (* dr arg-0)), :number 1}
                      {:args-def [], :exp (sin-osc (* dr 440)), :number 2}]})))

  (testing "split sexp with control bus"
    (is (= (sp/split-synth-body :hoge '(let [freq 440]
                                         [(section {:dur 2} (sin-osc (* :-freq dr (!! (if u3 freq (* 2 freq))))))
                                          (section (sin-osc (* dr :-freq)))]))

           '{:keep-node? false,
             :func-exp (let [freq 440] [{:arguments {:arg-0 (if u3 freq (* 2 freq))}, :arg-names [:arg-0], :arg-values [(if u3 freq (* 2 freq))], :dur 2, :number 1}
                                        {:arguments {}, :arg-names [], :arg-values [], :dur 1, :number 2}]),
             :synths [{:args-def [arg-0 0], :exp (sin-osc (* (overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-freq)) dr arg-0)), :number 1}
                      {:args-def [], :exp (sin-osc (* dr (overtone.sc.ugens/in:kr (llll.sc.sound-control/bus :hoge :-freq)))), :number 2}]}))))

(deftest queue-conversion-test
  (testing "convert"
    (let [xs (->> (sp/split-synth-body :hoge '[(section (sin-osc))
                                         (section {:dur 2} (saw (!! 400)))
                                         (section {:dur 3} (pulse (!! 200) (!! 0.5)))])
                  :func-exp
                  (sp/convert->Queue 12 (fn [n d args] #(str n "|" d "|" (join "/" args))))
                  :actions)]
      (is (= (map :tick xs) [0 2 6]))
      (is (= (map #((:func %)) xs) ["1|2|" "2|4|:arg-0/400" "3|6|:arg-0/200/:arg-1/0.5"])))))

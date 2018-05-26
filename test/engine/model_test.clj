(ns llll.engine.model-test
  (:require [clojure.test :refer :all]
            [llll.engine.variables :as v]
            [llll.engine.model :refer :all]))

(def ^:private take-while-period #'llll.engine.model/take-while-period)
(def ^:private merge-queue  #'llll.engine.model/merge-queue)

(defn- str-actions [queue]
  (->> (:actions queue)
       (map (fn [{:keys [tick func]}] (str tick ":" (func) ",")))
       (apply str)))

(deftest take-while-period-test
  (let [params [{:ticks-in-period 3 :result-consumed "" :remain-dur 2 :result-remain "0:?,1:!,"}
                {:ticks-in-period 5 :result-consumed "3:?,4:!," :remain-dur 0 :result-remain ""}
                {:ticks-in-period 6 :result-consumed "3:?,4:!,"
                 :remain-dur 2 :result-remain "1:1,"}
                {:ticks-in-period 10 :result-consumed "3:?,4:!,7:1,"
                 :remain-dur 1 :result-remain "0:2,"}
                {:ticks-in-period 11 :result-consumed "3:?,4:!,7:1,10:2,"
                 :remain-dur 0 :result-remain ""}
                {:ticks-in-period 20 :result-consumed "3:?,4:!,7:1,10:2,11:3,12:3,19:4,"
                 :remain-dur 0 :result-remain ""}
                {:ticks-in-period 21 :result-consumed "3:?,4:!,7:1,10:2,11:3,12:3,19:4,"
                 :remain-dur 2 :result-remain "1:5,"}
                ]]
    (doseq [{:keys [ticks-in-period remain-dur result-remain result-consumed]} params]
      (testing (str "take:" ticks-in-period)
        (let [initial-queue (->Queue 5 [(->Action 3 (fn [] "?"))
                                        (->Action 4 (fn [] "!"))])
              generator (fn [state]
                          (if (= (mod state 3) 0)
                            (->Queue 6 [(->Action 0 #(str state))
                                        (->Action 1 #(str state))])
                            (->Queue 3 [(->Action 2 #(str state))])))
              line (->Line :hoge initial-queue generator 0 inc)
              {new-line :line consumed :queue} (take-while-period ticks-in-period line)
              remain (:queue new-line)]
          (is (= (:dur consumed) ticks-in-period))
          (is (= (str-actions consumed) result-consumed))
          (is (= (:dur remain) remain-dur))
          (is (= (str-actions remain) result-remain)))))))

(deftest take-repeatedly-test
  (let [each-take-tick 4
        each-give-tick 6
        num-iterate 5]
    (testing (str "take:" each-take-tick)
      (let [initial-queue (->Queue each-give-tick [(->Action (dec each-give-tick) (fn [] "!"))])
            generator (fn [state]
                        (->Queue each-give-tick (map (fn [i] (->Action i #(str state)))
                                                     (range 0 each-give-tick))))
            line (->Line :hoge initial-queue generator 0 inc)
            buf (atom (->Queue 0 []))]
        (dorun (take (+ 2 num-iterate) (iterate (fn [{:keys [line queue]}]
                                                  (swap! buf #(merge-queue % queue))
                                                  (take-while-period each-take-tick line))
                                                {:queue (->Queue 0 []) :line line})))
        (is (= (map :tick (:actions @buf))
               (range (dec each-give-tick)
                      (+ -1 (dec each-give-tick) (* each-take-tick (dec num-iterate))))))
        (is (= ((-> @buf :actions last :func)) "3"))))))

(deftest take-while-without-state-update
  (let [{:keys [line queue]} (take-while-period 10
                               (make-line :test
                                       (fn [state] (->Queue 3 [(->Action 0 (fn [] state))]))
                                       "!"
                                       nil))]
    (is (= (:state line) "!"))
    (is (= (str-actions queue) "0:!,3:!,6:!,9:!,"))))


(deftest take-while-empty-queue
  (let [{:keys [line queue]} (take-while-period 5
                               (->Line :test
                                       (->Queue 8 [])
                                       (fn [state] (->Queue 8 [(->Action 0 (fn [] state))]))
                                       1
                                       inc))]
    (is (= (:state line) 1))
    (is (= (str-actions queue) ""))
    (is (= (:dur (:queue line)) 3))))

(deftest take-while-without-generator
  (let [{:keys [line queue]} (take-while-period 6
                               (->Line :test
                                       (->Queue 10 [(->Action 0 (fn [] "!"))
                                                    (->Action 4 (fn [] "?"))])
                                       nil
                                       "!"
                                       nil))]
    (is (= (:state line) "!"))
    (is (= (str-actions queue) "0:!,4:?,"))
    (is (= (:dur (:queue line)) 4))))

(deftest take-while-return-empty-queue
  (let [{:keys [line queue]} (take-while-period 6
                               (->Line :test
                                       (->Queue 5 [(->Action 0 (fn [] "!"))
                                                    (->Action 4 (fn [] "?"))])
                                       nil
                                       "!"
                                       nil))]
    (is (= (:state line) "!"))
    (is (= (str-actions queue) "0:!,4:?,"))
    (is (nil? (:queue line)))))

(deftest calcurate-schedule-test
  (dorun (map (fn [beats-per-osc expected-state run-result]
                (testing (str "hoge" expected-state)
                  (v/initialize 120 beats-per-osc 4)
                  (let [initial-queue (->Queue 5 [(->Action 1 (fn [] "!"))])
                        generator (fn [state]
                                    (->Queue state (if (= (mod state 2) 1)
                                                     [(->Action (dec state) #(str state))]
                                                     [])))
                        line (->Line :hoge initial-queue generator 0 inc)
                        {:keys [line schedules]} (calcurate-next-schedules line)]
                    (is (= (:state line) expected-state))
                    (is (= (apply str (map #((:func %)) schedules)) run-result)))))
              [1 2 8 16]
              [0 2 7 11]
              ["!" "!1" "!135" "!13579"])))


(deftest merge-line-test
  (let [base (make-line :hoge (fn [_] (->Queue 0 [])) {:x 1} #(update % :x inc))]
    (testing "merge empty"
      (is (= (merge-line base (->Line nil nil nil nil nil)) base))
      (is (= (merge-line (->Line :hoge nil nil nil nil) base) base)))

    (testing "replace functions"
      (let [new-line (merge-line base (->Line nil nil (fn [_] "ok") nil (fn [_] "ok 2")))]
        (is (= ((:queue-gen-func new-line) nil) "ok"))
        (is (= ((:state-update-func new-line) nil) "ok 2"))))

    (testing "merge state"
      (let [new-line (merge-line base (->Line nil nil nil {:x -1 :y 3} nil))]
        (is (= (:state new-line) {:x 2 :y 3}))))

    (testing "merge queue"
      (let [new-line (merge-line (->Line nil (->Queue 10 [(->Action 0 (fn [] "0"))
                                                          (->Action 3 (fn [] "3"))
                                                          (->Action 4 (fn [] "4"))])
                                         nil nil nil)
                                 (->Line nil (->Queue 5 [(->Action 2 (fn [] "2"))
                                                         (->Action 5 (fn [] "5"))])
                                         nil nil nil))]
        (is (= (get-in  new-line [:queue :dur]) 10))
        (is (= (str-actions (get-in  new-line [:queue])) "0:0,2:2,3:3,4:4,5:5,")))

      (let [new-line (merge-line (->Line nil (->Queue 5 [(->Action 2 (fn [] "2"))
                                                         (->Action 5 (fn [] "5"))])
                                         nil nil nil)
                                 (->Line nil (->Queue 10 [(->Action 0 (fn [] "0"))
                                                          (->Action 3 (fn [] "3"))
                                                          (->Action 4 (fn [] "4"))])
                                         nil nil nil))]
        (is (= (get-in  new-line [:queue :dur]) 10))
        (is (= (str-actions (get-in  new-line [:queue])) "0:0,2:2,3:3,4:4,5:5,"))))

    (testing "merge with other field"
      (let [new-line (merge-line (assoc (->Line nil (->Queue 1 [(->Action 0 (fn [] "0"))])
                                                nil nil nil)
                                        :fuga 5 :hoge 2 :yyy 20)
                                 (assoc (->Line nil nil nil nil nil)
                                        :hoge 10 :fuga nil :zzzz "a"))]
        (is (= (get-in  new-line [:queue :dur]) 1))
        (is (= (str-actions (get-in  new-line [:queue])) "0:0,"))
        (is (= (:hoge new-line) 10))
        (is (= (:fuga new-line) 5))
        (is (= (:yyy new-line) 20))
        (is (= (:zzzz new-line) "a"))))

    ))

(deftest once-queue-modifier-test
  (testing "once-queue-modifier-test"
    (let [line (make-line :test (fn [state] (->Queue 3 [(->Action 0 (fn [] (str state "_1")))
                                                                        (->Action 2 (fn [] (str state "_2")))]))
                                          1 inc)
          line (put-once-event line (fn [state] (str "event" state "!")))]
      (is (= (str-actions (:queue (take-while-period 10 line)))
             "0:1_1,2:1_2,3:event3!,3:3_1,5:3_2,6:4_1,8:4_2,9:5_1,")))))

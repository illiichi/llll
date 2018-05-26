(ns llll.transition-dsl-parser-test
  (:require [clojure.test :refer :all]
            [llll.clay.transition-dsl :refer :all]
            [clojure.string :refer [join]]
            [llll.clay.clay :as c]))

(defn str-result [{:keys [type num operator from to] :as x}]
  (case type
    :rest (str "R" [num])
    :set (str operator "[" num "]" to)
    :move (str operator
               "[" num "]"
               (when from (str from "=>"))
               to)
    (str x)))

(def ^:private parse #'llll.clay.transition-dsl/parse)

(deftest parse-test
  (doseq [{:keys [input expected]}
          [{:input '(> 3)                      :expected ">[1]3"}
           {:input '(10 > 3)                   :expected ">[1]10=>3"}
           {:input '(> (+ 2 3) >>[4] n)        :expected ">[1](+ 2 3), >>[4](+ 2 3)=>n"}
           {:input '(>[n] (+ x 3) >>| -10)     :expected ">[n](+ x 3), >>[:end](+ x 3)=>-10"}
           {:input '([3] > 3)                  :expected "R[3], >[1]3"}
           {:input '(1 > 3 >> 4)               :expected ">[1]1=>3, >>[1]3=>4"}
           {:input '(1 > 2 3 >> 4)             :expected ">[1]1=>2, >>[1]3=>4"}
           {:input '(1 - 3 >> 4)               :expected "-[1]1, >>[1]3=>4"}
           {:input '(1 - 2 - 3 > 4)            :expected "-[1]1, -[1]2, >[1]3=>4"}
           {:input '([2] 1 >[2] 3 >> 4)        :expected "R[2], >[2]1=>3, >>[1]3=>4"}
           {:input '(1 >[5] 3 4)               :expected ">[5]1=>3, -[0]4"}
           {:input '(1 >[5] 3 - 4)             :expected ">[5]1=>3, R[1], -[0]4"}
           {:input '(1 >[5] 3 [6] 4)           :expected ">[5]1=>3, R[6], -[0]4"}
           {:input '(1 [2] 3 >> 4)             :expected "-[2]1, >>[1]3=>4"}
           {:input '(1 - 3)                    :expected "-[1]1, -[0]3"}
           {:input '(1 [2] 3)                  :expected "-[2]1, -[0]3"}
           {:input '(1)                        :expected "-[0]1"}
           {:input '([2] 1)                    :expected "R[2], -[0]1"}
           {:input '(1 > 3 [2] >> 4)           :expected ">[1]1=>3, R[2], >>[1]3=>4"}
           {:input '(1 > 3 [2] >> [n] 4 >| x)  :expected ">[1]1=>3, R[2], >>[n]3=>4, >[:end]4=>x"}]]
    (testing (str "transit" input)
      (is (= expected (join ", " (map str-result (parse input))))))))

(deftest illigal-input-test
  (doseq [input ['(1 > 3 > [2])
                 '(1 > 3 >)
                 '(1 > [2] [1] 3)
                 '(1 - 2 3 > 4)
                 '(1 - 2 >)
                 '(1 > 2 3 >)
                 '(1 - [3] 2)
                 '(1 [3] - 2)
                 '(3 >)
                 '(1 2)]]
    (testing (str "try parse " input)
      (is (thrown? AssertionError (parse input))))))

(def ^:private separate-with #'llll.clay.transition-dsl/separate-with)

(deftest separate-test
  (doseq [{:keys [input expected extra]}
          [{:input [2 3] :expected [[1 2] [3 4 5]]}
           {:input [2 3 :end] :expected [[1 2] [3 4 5] [6 7 8 9]]}
           {:input [2 3 4] :expected [[1 2] [3 4 5] [6 7 8 9]]}
           {:input [2 3 5] :expected [[1 2] [3 4 5] [6 7 8 9]]}
           {:input [2 3 5 3] :expected [[1 2] [3 4 5] [6 7 8 9] []]}
           {:input [2 3 5 :end] :expected [[1 2] [3 4 5] [6 7 8 9] []]}
           {:input [5 1 0] :expected [[1 2 3 4 5] [6]] :extra [0 1]}
           {:input [4 0] :expected [[1 2 3 4]] :extra [1]}
           {:input [0] :expected [[]] :extra [0]}
           {:input (map :num (parse '(> [4] 2 >> [2] 1 >| 10))) :expected [[1 2 3 4] [5 6] [7 8 9]]}
           {:input (map :num (parse '(> [4] 2 [2] > 10))) :expected [[1 2 3 4] [5 6] [7]]}
           ]]
    (testing (str "separate " input)
      (let [input (map (fn [x] {:num x}) input)
            result (separate-with input (range 1 10))]
        (is (= (map :children result) expected))
        (when extra
          (is (= (map #(-> % :extra-transition count) result) extra)))))))

(deftest assign-test
  (doseq [{:keys [input expected]}
          [{:input '(> [2] 1) :expected "0:3/4"}
           {:input '(0 > [2] 1) :expected "0:3/4"}
           {:input '(0 > 1 > [2] 3) :expected "0:1/2,1/2:7/8"}
           {:input '(0 > 1 >>| 3) :expected "0:1/2,1/2:1"}
           {:input '(0 > 1 [2] >>| 3) :expected "0:1/2,7/8:1"}
           {:input '(0 > 1 [3] 3) :expected "0:1/2,15/16:15/16"}
           {:input '(0 > [4] 1 [3] 3) :expected "0:15/16,1:1"}
           {:input '(0 [7] 3) :expected "0:1,1:1"}]]
    (testing (str "assign " input)
      (let [result (assign-transition (parse input) (c/create-with-weight [0 1/2 1/4 1/8 1/16 1/32 1/32]))]
        (is (= (->> result
                    :children
                    (map (fn [{:keys [:start :end]}] (str start ":" end)))
                    (join ","))
               expected))))))

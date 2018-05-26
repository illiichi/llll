(ns llll.conversion-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [llll.clay.intermediate :as im]
            [llll.clay.clay :as c]
            [llll.clay.conversions :as conv]
            [llll.clay.clay-dsl-parser :as p]))

(deftest intermediate->clay-conversion-test
  (testing "no nest"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                                (im/append :a)
                                                (im/repeat-times 4)
                                                (im/assign-weight 8)
                                                (im/append :b)
                                                (im/append :c)))
          expected (->> (c/create-with-weight [0 2/10 2/10 2/10 2/10 1/10 1/10])
                        (c/assign {} :sym [:a :a :a :a :b :c]))]
      (is (= actual expected))))

  (testing "nested"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                                (im/append :a)
                                                (im/repeat-times 4)
                                                (im/assign-weight 8)
                                                (im/append (-> (im/->Root)
                                                            (im/append :b)
                                                            (im/append :c)))))
          expected (->> (c/create-with-weight [0 2/10 2/10 2/10 2/10 1/10 1/10])
                        (c/assign {} :sym [:a :a :a :a :b :c]))]
      (is (= actual expected))))

  (testing "more nested"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                                (im/append (-> (im/->Root)
                                                             (im/append :a)
                                                             (im/append :b)))
                                             (im/assign-weight 1)
                                             (im/repeat-times 2)
                                             (im/append (-> (im/->Root)
                                                            (im/append (-> (im/->Root)
                                                                        (im/append :c)))
                                                            (im/repeat-times 2)))))
          expected (->> (c/create-with-weight [0 1/8 1/8 1/8 1/8 1/4 1/4])
                        (c/assign {} :sym [:a :b :a :b :c :c]))]
      (is (= actual expected)))))

(deftest intermediate->clay-conversion-test-with-rest
  (testing "start and end"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                              (im/append :-)
                                              (im/append :a)
                                              (im/append :b)
                                              (im/append :c)
                                              (im/append :-)))
          expected (c/->Clay [(c/->Child 1/5 2/5 :a)
                              (c/->Child 2/5 3/5 :b)
                              (c/->Child 3/5 4/5 :c)])]
      (is (= actual expected))))

  (testing "mid (repeat)"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                              (im/append :a)
                                              (im/append (-> (im/->Root)
                                                             (im/append :-)
                                                             (im/repeat-times 4)))
                                              (im/append :c)
                                              ))
          expected (c/->Clay [(c/->Child 0 1/6 :a)
                              (c/->Child 5/6 1 :c)])]
      (is (= actual expected))))
  (testing "mid (weighted"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                              (im/append :a)
                                              (im/append (-> (im/->Root)
                                                             (im/append :-)
                                                             (im/assign-weight 4)))
                                              (im/append :c)
                                              ))
          expected (c/->Clay [(c/->Child 0 1/6 :a)
                              (c/->Child 5/6 1 :c)])]
      (is (= actual expected)))))

(deftest intermediate->clay-conversion-test-with-0-weighted
  (testing "start"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                              (im/append :a)
                                              (im/assign-weight 0)
                                              (im/append :b)
                                              (im/assign-weight 3)
                                              (im/append :c)))
          expected (c/->Clay [(c/->Child 0 3/4 :b)
                              (c/->Child 3/4 1 :c)])]
      (is (= actual expected))))

  (testing "mid and end"
    (let [actual (conv/intermediate->clay (-> (im/->Root)
                                              (im/append :a)
                                              (im/append (-> (im/->Root)
                                                             (im/append :b1)
                                                             (im/append :b2)
                                                             (im/assign-weight 0)
                                                             (im/append :b3)))
                                              (im/append (-> (im/->Root)
                                                             (im/append :c)
                                                             (im/assign-weight 0)))))
          expected (c/->Clay [(c/->Child 0 1/2 :a)
                              (c/->Child 1/2 1 :b3)])]
      (is (= actual expected)))))


(deftest dsl-conversion-test
  (testing "no-nest"
    (let [actual (eval (conv/dsl->intermediate-exp '(:a|2 :b :c*2)))
          expected (-> (im/->Root)
                       (im/append :a {} 2)
                       (im/append :b)
                       (im/append :c)
                       (im/append :c))]
      (is (= actual expected))))

  (testing "nested"
    (let [actual (eval (conv/dsl->intermediate-exp '(:a|2 [:b :c] *2 :d*2|6)))
          expected (-> (im/->Root)
                       (im/append :a {} 2)
                       (im/append :b)
                       (im/append :c)
                       (im/append :b)
                       (im/append :c)
                       (im/append :d {} 3)
                       (im/append :d {} 3))]
      (is (= actual expected))))

  (testing "more nested"
    (let [actual (eval (conv/dsl->intermediate-exp '([:a|2 :a2] :b :c *2 [:d [:e :f] |4])))
          expected (-> (im/->Root)
                       (im/append :a {} 2)
                       (im/append :a2)
                       (im/append :b)
                       (im/append :c)
                       (im/append :c)
                       (im/append :d)
                       (im/append :e {} 2)
                       (im/append :f {} 2))]
      (is (= actual expected)))))


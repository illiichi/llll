(ns llll.intermediate-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [llll.clay.intermediate :as im]))

(declare str-intm)

(defn str-child [{:keys [sym weight params children] :as intm}]
  (if children
    (str-intm intm)

    (if (empty? params)
      (str sym weight)
      (str sym weight params))))

(defn str-intm [{:keys [type weight elements children]}]
  (str "[" weight "|" (join "," (map str-child (or children elements))) "]"))

(deftest intm-append-test
  (testing "append sym"
    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b))]
      (is (= (str-intm intm) "[2|:a1,:b1]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b {:x 10} 4))]
      (is (= (str-intm intm) "[5|:a1,:b4{:x 10}]"))))
  (testing "append intm"
    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append (-> (im/->Root)
                                 (im/append :d)
                                 (im/append :e))))]
      (is (= (str-intm intm) "[3|:a1,[2|:d1,:e1]]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b)
                   (im/append (im/->Root)))]
      (is (= (str-intm intm) "[2|:a1,:b1,[0|]]"))))
  (testing "append if sym is nil"
    (let [intm (-> (im/->Root)
                   (im/append nil)
                   (im/append nil))]
      (is (= (str-intm intm) "[2|1,1]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b)
                   (im/append nil {} 3)
                   (im/append nil {:x 10} 10))]
      (is (= (str-intm intm) "[15|:a1,:b1,3,10{:x 10}]"))))
  (testing "append into empty intermediate"
    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                      (im/append :a))))]
      (is (= (str-intm intm) "[1|[1|:a1]]")))))

(deftest intm-repeat-test
  (testing "repeat"
    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/repeat-times 4))]
      (is (= (str-intm intm) "[4|:a1,:a1,:a1,:a1]")))

    (let [intm (-> (im/->Root)
                   (im/append nil)
                   (im/repeat-times 4))]
      (is (= (str-intm intm) "[4|1,1,1,1]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b)
                   (im/repeat-times 3))]
      (is (= (str-intm intm) "[6|:a1,:b1,:a1,:b1,:a1,:b1]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b {} 2)
                   (im/repeat-times 3))]
      (is (= (str-intm intm) "[9|:a1,:b2,:a1,:b2,:a1,:b2]")))

    (let [intm (-> (im/->Root)
                   (im/repeat-times 4))]
      (is (= (str-intm intm) "[0|]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/repeat-times 3)
                   (im/append :b {} 2)
                   (im/repeat-times 2))]
      (is (= (str-intm intm) "[10|:a1,:a1,:a1,:b2,:a1,:a1,:a1,:b2]")))))

(deftest assign-weight-test
  (testing "assign weight"
    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/append :b)
                   (im/assign-weight 4))]
      (is (= (str-intm intm) "[4|:a2,:b2]")))

    (let [intm (-> (im/->Root)
                   (im/append nil)
                   (im/assign-weight 2)
                   (im/repeat-times 3)
                   (im/append nil {} 4))]
      (is (= (str-intm intm) "[10|2,2,2,4]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/repeat-times 2)
                   (im/append :b {} 2)
                   (im/assign-weight 2)
                   (im/repeat-times 2))]
      (is (= (str-intm intm) "[4|:a1/2,:a1/2,:b1,:a1/2,:a1/2,:b1]")))))

(deftest nested-intm
  (testing "creation"
    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a))))]
      (is (= (str-intm intm) "[1|[1|:a1]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append (-> (im/->Root)
                                               (im/append :a))))))]
      (is (= (str-intm intm) "[1|[1|[1|:a1]]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a)
                                 (im/append :b))))]
      (is (= (str-intm intm) "[2|[2|:a1,:b1]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a)))
                   (im/append :b))]
      (is (= (str-intm intm) "[2|[1|:a1],:b1]"))))

  (testing "append"
    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/assign-weight 4)
                   (im/append
                    (-> (im/->Root)
                        (im/append :a)
                        (im/append :b {} 2))))]
      (is (= (str-intm intm) "[7|:a4,[3|:a1,:b2]]")))

    (let [intm (-> (-> (im/->Root)
                       (im/append (-> (im/->Root)
                                     (im/append :a))))
                   (im/append
                    (-> (im/->Root)
                        (im/append :a)
                        (im/assign-weight 2)
                        (im/append :b {} 2))))]
      (is (= (str-intm intm) "[5|[1|:a1],[4|:a2,:b2]]")))

    (let [intm (-> (im/->Root)
                   (im/append :a)
                   (im/assign-weight 6)
                   (im/append
                    (-> (im/->Root)
                        (im/append :a)
                        (im/append :b {} 2)
                        (im/assign-weight 3)))
                   (im/append
                    (-> (im/->Root)
                        (im/append :c)
                        (im/append :d {} 2)
                        (im/assign-weight 6))))]
      (is (= (str-intm intm) "[15|:a6,[3|:a1,:b2],[6|:c2,:d4]]"))))

  (testing "assign weight"
    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a)))
                   (im/assign-weight 4))]
      (is (= (str-intm intm) "[4|[4|:a4]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a)
                                 (im/append :b)
                                 (im/assign-weight 1/4)))
                (im/assign-weight 2)
                (im/assign-weight 8))]
      (is (= (str-intm intm) "[8|[8|:a4,:b4]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append :a)
                                 (im/append :b)
                                 (im/assign-weight 1)))
                   (im/append (-> (im/->Root)
                                 (im/append :c)
                                 (im/append :d {} 2)
                                 (im/assign-weight 3)))
                   (im/assign-weight 16))]
      (is (= (str-intm intm) "[16|[4|:a2,:b2],[12|:c4,:d8]]")))

    (let [intm (-> (im/->Root)
                   (im/append (-> (im/->Root)
                                 (im/append nil)
                                 (im/append nil)
                                 (im/assign-weight 1)))
                   (im/append (-> (im/->Root)
                                 (im/append nil)
                                 (im/append nil {:x 1} 2)
                                 (im/assign-weight 3)))
                   (im/assign-weight 16))]
      (is (= (str-intm intm) "[16|[4|2,2],[12|4,8{:x 1}]]")))))


(deftest join-test
  (testing "join empty"
    (let [intm (im/join [(im/->Root)
                        (im/->Root)])]
      (is (= (str-intm intm) "[0|]")))

    (let [intm (im/join [(im/->Root)
                        (-> (im/->Root)
                            (im/append nil))])]
      (is (= (str-intm intm) "[1|1]")))

    (let [intm (im/join [(-> (im/->Root)
                            (im/append nil))
                        (im/->Root)])]
      (is (= (str-intm intm) "[1|1]"))))
  
  (testing "join sym only"
    (let [intm (im/join [(-> (im/->Root)
                            (im/append :a))
                        (-> (im/->Root)
                            (im/append nil)
                            (im/append :c))])]
      (is (= (str-intm intm) "[3|:a1,1,:c1]")))

    (let [intm (im/join [(-> (im/->Root)
                            (im/append :a))
                        (-> (im/->Root)
                            (im/append nil {:x 1} 4)
                            (im/append :c {:y :a} 2))
                        (-> (im/->Root)
                            (im/append nil)
                            (im/append :e))])]
      (is (= (str-intm intm) "[9|:a1,4{:x 1},:c2{:y :a},1,:e1]"))))

  (testing "join nested"
    (let [intm (im/join [(-> (im/->Root)
                            (im/append :a))
                        (-> (im/->Root)
                            (im/append (-> (im/->Root)
                                          (im/append :b)))
                            (im/append :c))])]
      (is (= (str-intm intm) "[3|:a1,[1|:b1],:c1]"))))

  (testing "join repeated and weighted"
    (let [intm (im/join [(-> (im/->Root)
                            (im/append :a)
                            (im/repeat-times 3))
                        (-> (im/->Root)
                            (im/append (-> (im/->Root)
                                          (im/append :b)))
                            (im/append :c)
                            (im/assign-weight 12)
                            (im/repeat-times 2))])]
      (is (= (str-intm intm) "[27|:a1,:a1,:a1,[6|:b6],:c6,[6|:b6],:c6]")))))

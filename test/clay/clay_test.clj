(ns llll.clay-test
  (:require [clojure.test :refer :all]
            [llll.engine.variables :as v]
            [llll.clay.clay :as c]))

(defn- str-clay [{:keys [children]}]
  (->> children
       (sort-by :start)
       (map (fn [{:keys [start end sym]}]
              (str start "-" end sym ",")))
       (apply str)))

(deftest concat-clay
  (testing "concat 2"
    (let [cray (c/concat-clay (c/create-clay :a)
                              (c/create-clay :b))]
      (is (= (->> cray :children count) 2))
      (is (= (str-clay cray) "0-1/2:a,1/2-1:b,"))))

  (testing "concat 3"
    (let [cray (c/concat-clay (c/create-clay :a)
                              (c/create-clay :b)
                              (c/create-clay :c))]
      (is (= (->> cray :children count) 3))
      (is (= (str-clay cray) "0-1/3:a,1/3-2/3:b,2/3-1:c,"))))
  (testing "concat twice"
    (let [cray (c/concat-clay
                (c/create-clay :a)
                (c/concat-clay (c/create-clay :b)
                               (c/create-clay :c)))]
      (is (= (->> cray :children count) 3))
      (is (= (str-clay cray) "0-1/2:a,1/2-3/4:b,3/4-1:c,")))))

(defn- str-queue [{:keys [actions]}]
  (->> actions
       (map (fn [{:keys [tick func]}]
              (str tick (func) ",")))
       (apply str)))

(defn- dummy-func [sym]
  (fn [dur params]
    (fn [] (str (name sym) dur
                (if (empty? params) "" params)))))


(deftest burn-test
  (v/initialize 120 4 4)
  (let [symbol-table {:symbol-table dummy-func}
        interval (* 6 (/ v/*bpm* 60) v/*ticks-per-beat*)]
    (testing "single-cray"
      (let [queue (c/burn symbol-table interval (c/create-clay :a))]
        (is (= (:dur queue) interval))
        (is (= (str-queue queue) "0a6,"))))
    (testing "two-cray"
      (let [queue (c/burn symbol-table interval (c/concat-clay (c/create-clay :a)
                                                               (c/create-clay :b)))]
        (is (= (:dur queue) interval))
        (is (= (str-queue queue) (str "0a3," (* 1/2 interval) "b3,")))))
    (testing "three-cray"
      (let [queue (c/burn symbol-table interval (c/concat-clay (c/create-clay :a)
                                                               (c/create-clay :b)
                                                               (c/create-clay :c)))]
        (is (= (:dur queue) interval))
        (is (= (str-queue queue) (str "0a2," (* 1/3 interval) "b2," (* 2/3 interval) "c2,")))))

    (testing "with params"
      (let [queue (c/burn symbol-table interval (c/concat-clay (c/create-clay 1/2 1 :a {:x 1})
                                                               (c/create-clay 1/2 1 :a )
                                                               (c/create-clay 1/4 1/2 :b)
                                                               (c/create-clay 0 1/2 :b {:y 2})))]
        (is (= (:dur queue) interval))
        (is (= (str-queue queue) "6a3/4{:x 1},12a3/2,24b3/2,36b3/4{:y 2},"))))))

(deftest weighted-cray-creation
  (testing "exact 1"
    (let [xs (:children (c/create-with-weight [0 1/4 1/4 1/2]))]
      (is (= (map :start xs) [0 1/4 1/2]))
      (is (= (map :end xs)   [1/4 1/2 1]))))

  (testing "less than 1"
    (let [xs (:children (c/create-with-weight [0 1/4 1/4] :a))]
      (is (= (map :start xs) [0 1/4 1/2]))
      (is (= (map :end xs)   [1/4 1/2 1]))
      (is (= (map :sym xs)   [:a :a :a]))))

  (testing "greater than 1"
    (let [xs (:children (c/create-with-weight [0 1/4 1/4 1/3 1/2] :a {:x 1}))]
      (is (= (map :start xs) [0 1/4 1/2 5/6]))
      (is (= (map :end xs)   [1/4 1/2 5/6 4/3]))
      (is (= (map :sym xs)   [:a :a :a :a]))
      (is (= (map #(get-in % [:params :x]) xs)   [1 1 1 1])))))

(deftest assign-test
  (testing "no cycle"
    (let [clay (->> (c/create-with-weight (cons 0 (repeat 8 1/8)))
                    (c/assign {} :sym [:a :b nil :c nil])
                    (c/assign {} :freq [nil 100 200 nil 400]))
          xs (:children clay)]
      (is (= (map :sym xs)  (concat [:a :b nil :c] (repeat 4 nil))))
      (is (= (map #(get-in % [:params :freq]) xs) (concat [nil 100 200 nil 400] (repeat 3 nil))))))

  (testing "with cycle"
    (let [clay (->> (c/create-with-weight (cons 0 (repeat 8 1/8)))
                    (c/assign {:cycle? true} :sym [:a :b nil :c nil])
                    (c/assign {:cycle? true} :freq [nil 100 200 nil 400]))
          xs (:children clay)]
      (is (= (map :sym xs)  [:a :b nil :c nil :a :b nil]))
      (is (= (map #(get-in % [:params :freq]) xs) [nil 100 200 nil 400 nil 100 200]))))

  (testing "with overwrite"
    (let [clay (->> (c/create-with-weight (cons 0 (repeat 8 1/8)))
                    (c/assign {:cycle? true} :sym [:a :b nil :c nil])
                    (c/assign {:cycle? true} :freq [nil 100 200 nil 400])
                    (c/assign {} :sym [:d nil :e])
                    (c/assign {} :freq [-100 -200 -300]))
          xs (:children clay)]
      (is (= (map :sym xs)  [:d :b :e :c nil :a :b nil]))
      (is (= (map #(get-in % [:params :freq]) xs) [-100 -200 -300 nil 400 nil 100 200])))))

(deftest merge-test
  (testing "merge single clay"
    (let [clay (c/merge-clay (c/create-clay :a)
                             (c/create-clay :b))]
      (is (= (str-clay clay) "0-1:a,0-1:b,")))
    (let [clay (c/merge-clay (c/create-with-weight [0 1/4 1/4 1/4 1/4] :a)
                             (c/create-with-weight [1/2 1/6 1/6 1/6] :b)
                             (->> (c/create-clay :c)))]
      (is (= (str-clay clay) "0-1/4:a,0-1:c,1/4-1/2:a,1/2-3/4:a,1/2-2/3:b,2/3-5/6:b,3/4-1:a,5/6-1:b,"))))

  (testing "merge collection of clay"
    (let [clay (c/merge-clay (map c/create-clay [:a :b]))]
      (is (= (str-clay clay) "0-1:a,0-1:b,")))

    (let [clay (c/merge-clay (map c/create-clay [:a :b])
                             (c/create-clay :c))]
      (is (= (str-clay clay) "0-1:a,0-1:b,0-1:c,")))

    (let [clay (c/merge-clay (for [x (range 1 3)
                                   y (range x)]
                               (c/create-with-weight (range 0 1 (/ 1 (+ x y))) :a ))
                             (map (fn [x] (->> (map #(c/create-clay (keyword (str % x))) ["x" "y"])))
                                  (range 3)))]
      (is (= (str-clay clay) "0-1:a,0-1/2:a,0-1/3:a,0-1:x0,0-1:y0,0-1:x1,0-1:y1,0-1:x2,0-1:y2,1/3-1:a,1/2-1:a,")))))

(deftest shift-test
  (testing "shift"
    (let [clay (->> (c/create-with-weight [0 1/4 1/4 1/4 1/4] :a)
                    (c/shift 1/4))]
      (is (= (str-clay clay) "1/4-1/2:a,1/2-3/4:a,3/4-1:a,1-5/4:a,")))

    (let [clay (->> (c/create-with-weight [1/2 1/4 1/4] :a)
                    (c/shift -1/4))]
      (is (= (str-clay clay) "1/4-1/2:a,1/2-3/4:a,")))))





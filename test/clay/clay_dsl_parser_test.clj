(ns llll.clay-dsl-parser-test
  (:require [clojure.test :refer :all]
            [llll.clay.clay-dsl-parser :refer :all]
            [clojure.string :refer [join]]))

(def ^:private split-token #'llll.clay.clay-dsl-parser/split-token)

(defn str-token [{:keys [type value as-sym as-value] :as m}]
  (case [type as-sym as-value]
    [:operand true nil] (str "sym=" value)
    [:operator nil nil] (str "op=" value)
    [:operand nil true] (str "v=" value)
    true (throw (RuntimeException. (str "invalid state:" m)))))

(deftest split-token-test
  (doseq [{:keys [input expected]}
          [{:input :a :expected "sym=:a"}
           {:input :- :expected "sym=:-"}
           {:input :abc34 :expected "sym=:abc34"}
           {:input :a-n- :expected "sym=:a-n-"}
           {:input :a2*-3.3|-4 :expected "sym=:a2,op=*,v=-3.3,op=|,v=-4"}
           {:input :**n2 :expected "sym=,op=**,v=n2"}
           {:input '||n2*0 :expected "op=||,v=n2,op=*,v=0"}
           {:input 'n2 :expected "v=n2"}
           {:input 'n-2000 :expected "v=n-2000"}
           {:input 10 :expected "v=10"}
           {:input -10.5 :expected "v=-10.5"}]]
    (testing (str "split:" input)
      (is (= (join "," (map str-token (split-token input))) expected)))))

(defn str-modifiers [xs]
  (apply str (map (fn [{:keys [operator value]}] (str operator value))
                  xs)))

(declare str-nodes)
(defn str-node [{:keys [sym children modifiers]}]
  (str (if children (str-nodes children) sym)
       (str-modifiers modifiers)))

(defn str-nodes [xs]
  (str "[" (join "," (map str-node xs)) "]"))

(deftest parser-test
  (doseq [{:keys [input expected]}
          [{:input '(:a1)                  :expected "[:a1]"}
           {:input '(:abcd :efg)                  :expected "[:abcd,:efg]"}
           {:input '(:a :b*2)                     :expected "[:a,:b*2]"}
           {:input '(:a :b *2)                    :expected "[:a,:b*2]"}
           {:input '(:a :b * 2)                    :expected "[:a,:b*2]"}
           {:input '(:a :b *n)                    :expected "[:a,:b*n]"}
           {:input '(:a :b * n)                    :expected "[:a,:b*n]"}
           {:input '(:a :b*0)                     :expected "[:a,:b*0]"}
           {:input '(:a :b * -1)                     :expected "[:a,:b*-1]"}
           {:input '(:a :- :b *n)                    :expected "[:a,:-,:b*n]"}
           {:input '(:abcd *m|n :*a4|5)           :expected "[:abcd*m|n,*a4|5]"}
           {:input '(:*2|3 >1 :dd*4|5 >>2 :ee)    :expected "[*2|3>1,:dd*4|5>>2,:ee]"}
           {:input '(:*2|abc >1 :d1|n2*n1 |n3)    :expected "[*2|abc>1,:d1|n2*n1|n3]"}]]
    (testing (str "simple" input)
      (is (= (str-nodes (parse input)) expected)))))

(deftest parse-nested-test
  (doseq [{:keys [input expected]}
          [{:input '([:c] *2)                     :expected "[[:c]*2]"}
           {:input '([:c]*2)                      :expected "[[:c]*2]"}
           {:input '([[:c] *2] *4)                :expected "[[[:c]*2]*4]"}
           {:input '(:a [[:c] *2] *4 :d)          :expected "[:a,[[:c]*2]*4,:d]"}
           {:input '([:a*2] *2 [[:c] *2] *4 :d)   :expected "[[:a*2]*2,[[:c]*2]*4,:d]"}
           {:input '([:a*2]*2 [[:c]*2]*4 :d)      :expected "[[:a*2]*2,[[:c]*2]*4,:d]"}
           {:input '([:a*2] *2 :c*2 *4 [:d])      :expected "[[:a*2]*2,:c*2*4,[:d]]"}
           {:input '(:a*a|b *c :b|d [:c|e *d :d] *e) :expected "[:a*a|b*c,:b|d,[:c|e*d,:d]*e]"}
           ]]
    (testing (str "nesting:" input)
      (is (= (str-nodes (parse input)) expected)))))


(deftest parse-sexp-contains-test
  (doseq [{:keys [input expected]}
          [{:input '(:c *(+ 2 3))                      :expected "[:c*(+ 2 3)]"}
           {:input '(:c * (+ 2 3))                     :expected "[:c*(+ 2 3)]"}
           {:input '((if true :a :b)*(+ 2 3))          :expected "[(if true :a :b)*(+ 2 3)]"}
           {:input '([(if b :a :b)] * (+ n 3) :b2)     :expected "[[(if b :a :b)]*(+ n 3),:b2]"}
           {:input '(:b2 [(if b :a :b)] * (+ n 3))     :expected "[:b2,[(if b :a :b)]*(+ n 3)]"}
           {:input '(:b2 [(if b :a :b) |2] * (+ n 3))  :expected "[:b2,[(if b :a :b)|2]*(+ n 3)]"}
           {:input '(:b(+ 1) [(if b :a :b)] * (+ n 3)) :expected "[:b,(+ 1),[(if b :a :b)]*(+ n 3)]"}]]
    (testing (str "nesting:" input)
      (is (= (str-nodes (parse input)) expected)))))



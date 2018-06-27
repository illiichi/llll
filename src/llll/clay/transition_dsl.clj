(ns llll.clay.transition-dsl
  (require [llll.clay.clay :as c]))

(defn value? [x]
  (or (list? x) (symbol? x) (number? x)))

(defn uni-operator? [x]
  (= '- x))

(defn bi-operator? [x]
  (#{'> '>> '>| '>>|} x))

(defn detect-type [x]
  (cond
    (uni-operator? x) :uni-operator
    (bi-operator? x) :bi-operator
    (vector? x) :vector
    (value? x) :value
    true (throw (RuntimeException. (str "Not allowed:" x)))))

(def state-transition
  {:start                {:value :left-value
                          :vector :rest
                          :bi-operator :bi-operator}
   :rest                 {:value :left-value
                          :bi-operator :bi-operator}
   :left-value           {:bi-operator :bi-operator
                          :uni-operator :uni-operator
                          :vector :rest}
   :right-value          {:uni-operator :rest
                          :bi-operator :bi-operator
                          :vector :rest
                          :value :left-value}
   :uni-operator         {:value :left-value}
   :bi-operator          {:value :right-value
                          :vector :bi-duration}
   :bi-duration          {:value :right-value}})

(defn- transit [xs]
  (->> xs
       (reduce (fn [[current-state :as acc] input]
                 (let [type (detect-type input)
                       transition (state-transition current-state)
                       next-state (some-> transition type)]
                   (assert next-state (str "no way from " current-state " : " input))
                   (cons next-state acc)))
               '(:start))
       (cons :finish)
       reverse
       rest))

(defn ->UnitaryOperation
  [v operator dur]
  {:type :set
   :operator (str operator)
   :to v
   :num (or dur 1)})

(defn ->BinaryOperation
  [l op dur r]
  (let [[_ op suffix] (re-find #"([>]+)(\|?)$" (str op))
        base {:type :move
              :operator op
              :to r
              :num (if (empty? suffix) (or dur 1) :end)}]
    (if (nil? l)
      base
      (assoc base :from l))))

(defn ->Rest [dur]
  {:type :rest :num (or dur 1)})

(defn pattern [pattern-seq f]
  (fn [context inputs state-seq]
    (loop [args []
           [x & xs :as inputs] inputs
           [state & state-rest :as state-seq] state-seq
           [pattern-exp & pattern-rest] pattern-seq]
      (if (nil? pattern-exp)
        (let [{new-context :context :keys [emit]} (apply f context args)]
          {:emit emit :context (or new-context context) :state-seq state-seq :inputs inputs})

        (let [[expected-state optional?] (if (list? pattern-exp)
                                           [(first pattern-exp) true]
                                           [pattern-exp false])]
          (cond
            (= state expected-state) (recur (conj args x) xs state-rest pattern-rest)
            optional? (recur (conj args nil) inputs state-seq pattern-rest)))))))

(def patterns
  [(pattern '[(:left-value) :bi-operator (:bi-duration) :right-value]
            (fn [{:keys [prev] :as context} l op dur r]
              {:emit (->BinaryOperation (or l prev) op (or (first dur) 1) r)
               :context {:prev r}}))
   (pattern '[:left-value :uni-operator]
            (fn [context l op] {:emit (->UnitaryOperation l op 1)}))
   (pattern '[:left-value :rest]
            (fn [context l dur] {:emit (->UnitaryOperation l '- (first dur))}))
   (pattern '[:left-value :finish]
            (fn [context l _] {:emit (->UnitaryOperation l '- 0)}))
   (pattern '[:rest]
            (fn [context dur] {:emit (->Rest (if (vector? dur) (first dur) 1))}))])

(defn parse [inputs]
  (letfn [(step [context inputs state-seq]
            (loop [[pattern-checker & rest-patterns] patterns]
              (when pattern-checker
                (or (pattern-checker context inputs state-seq)
                    (recur rest-patterns)))))]
    (loop [acc []
           context {}
           inputs inputs
           state-seq (transit inputs)]
      (if (empty? inputs) acc
          (let [{:keys [emit inputs state-seq context] :as result} (step context inputs state-seq)]
            (assert result (str "no way to consume: " inputs "(" state-seq ")"))
            (recur (conj acc emit) context inputs state-seq))))))

(defn- take-zeros [xs]
  ((juxt take-while drop-while) #(= 0 (:num %)) xs))


(defn- separate-with [transitions xs]
  (loop [acc []
         [transition & transition-rest] transitions
         xs xs]
    (let [num (:num transition)
          [zero-transitions transition-rest] (take-zeros transition-rest)
          fragment {:transition transition :extra-transition zero-transitions} ]
      (case num
        nil acc
        :end (conj acc (assoc fragment :children xs))
        (let [[t d] ((juxt take drop) num xs)]
          (recur (conj acc (assoc fragment :children t)) transition-rest d))))))

(defmulti operation->action-params :type)

(defmethod operation->action-params :set [{:keys [to]}]
  {:set to})

(defmethod operation->action-params :move [{:keys [operator from to]}]
  {:linear? (= operator ">") :v-prev from :v to})

(defmethod operation->action-params :rest [{:keys [operator from to]}]
  nil)

(defn- merge-as-single [{:keys [children transition extra-transition]}]
  (let [param (operation->action-params transition)]
    (if (not (empty? children))
      (let [f (first children)
            l (last children)
            extras (->> extra-transition
                        (map #(c/->Child (:end l) (:end l) (:sym l) (operation->action-params %))))]
        (->> (cons (if param (c/->Child (:start f) (:end l) (:sym f) param)) extras)
             (filter identity))))))

(defn assign-transition [transitions {:keys [children]}]
  (c/->Clay (mapcat merge-as-single (separate-with transitions children))))


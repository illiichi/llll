(ns llll.clay.conversions
  (require [llll.clay.clay :as c]
           [llll.clay.intermediate :as im]
           [llll.clay.clay-dsl-parser :as p]))

(defn- rest-sym? [sym]
  (= :- sym))

(defn intermediate->clay [{total-weight :weight :keys [type elements]}]
  (assert (= type :intermediate))
  (let [[result total] (->> elements
                            (mapcat (partial tree-seq im/branch? :children))
                            (filter (complement im/branch?))
                            (reduce (fn [[result acc-weight] {:keys [weight sym params]}]
                                      (let [next-weight (+ acc-weight weight)]
                                        [(if (or (rest-sym? sym) (= weight 0))
                                           result

                                           (conj result (c/->Child (/ acc-weight total-weight)
                                                                   (/ next-weight total-weight)
                                                                   sym params)))
                                         next-weight]))
                                    [[] 0]))]
    (assert (= total total-weight) (str "total not 1:" total))
    (c/->Clay result)))

(def operator-table {'* `im/repeat-times,
                     '| `im/assign-weight})

(defn- apply-operator [intm {:keys [operator value]}]
  (let [f (operator-table operator)]
    (assert (not (nil? f)))
    `(~f ~intm ~value)))

(defn- convert-parsed-node [{:keys [sym children modifiers]}]
  (let [new-intm (if children
                   `(im/join ~(mapv convert-parsed-node children))
                   `(im/append (im/->Root) ~sym))]
    (reduce apply-operator new-intm modifiers)))

(defn from-parsed [xs]
  `(im/join ~(mapv convert-parsed-node xs) ))

(defn dsl->intermediate-exp [xs]
  (-> xs p/parse from-parsed))

(defmacro parse [& xs]
  `(intermediate->clay ~(dsl->intermediate-exp xs)))

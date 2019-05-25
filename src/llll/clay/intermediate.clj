(ns llll.clay.intermediate
  (:require [llll.clay.clay-dsl-parser :as p]))

(defn ->Root
  ([] (->Root 0 []))
  ([weight elements]
   (assert (vector? elements))
   {:type :intermediate :weight weight :elements elements}))

(defn ->Branch [children weight]
  (assert (vector? children))
  {:children children :weight weight})

(defn ->Node
  ([sym] {:sym sym :params {} :weight 1})
  ([sym params] {:sym sym :params params :weight 1})
  ([sym params weight] {:sym sym :params params :weight weight}))

(defn branch? [{:keys [children]}] children)
(defn sym? [x]
  (or (keyword? x) (nil? x)))

(defn- append-sym
  ([intm sym] (append-sym intm sym {} 1))
  ([intm sym param weight]
   (-> intm
       (update :weight #(+ % weight))
       (update :elements #(conj % (->Node sym param weight))))))

(defn- append-intm
  [intm {:keys [weight elements]}]
  (-> intm
      (update :weight #(+ % weight))
      (update :elements #(conj % (->Branch elements weight)))))

(defn append
  ([intm sym param weight]
   (assert (sym? sym))
   (append-sym intm sym param weight))
  
  ([intm target]
   (if (sym? target)
     (append-sym intm target)
     (append-intm intm target))))

(defn- join-single
  [intm {:keys [weight elements]}]
  (-> intm
      (update :weight #(+ % weight))
      (update :elements #(vec (concat % elements)))))

(defn join [intms]
  (reduce join-single intms))

(defn repeat-times [intm n]
  (-> intm
      (update :weight #(* % n))
      (update :elements #(vec (apply concat (repeat n %))))))

(defn- update-weight-by-ratio [ratio x]
  (if (branch? x)
    (-> x
        (update :weight (partial * ratio))
        (update :children (partial mapv (partial update-weight-by-ratio ratio))))

    (update x :weight (partial * ratio))))

(defn assign-weight [{:keys [weight] :as intm} new-weight]
  (let [ratio (/ new-weight weight)]
    (-> intm
        (assoc :weight new-weight)
        (update :elements (partial mapv (partial update-weight-by-ratio ratio))))))



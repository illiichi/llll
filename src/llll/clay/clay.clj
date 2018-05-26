(ns llll.clay.clay
  (:require [llll.engine.variables :as v]
            [llll.engine.model :as m]))

(defn ->Child
  ([start end sym]
   {:start start :end end :sym sym :params {}})
  ([start end sym params]
   {:start start :end end :sym sym :params params}))

(defn ->Clay [children]
  {:type :clay
   :children children})

(defn create-clay
  ([]
   (create-clay 0 1 nil {}))
  ([sym]
   (create-clay 0 1 sym {}))
  ([sym params]
   (create-clay 0 1 sym params))
  ([start end sym]
   (create-clay 0 1 sym {}))
  ([start end sym params]
   (->Clay [(->Child start  end  sym  params)])))

(defn create-with-weight
  ([weights]
   (create-with-weight weights nil {}))
  ([weights sym]
   (create-with-weight weights sym {}))
  ([weights sym params]
   (let [xs (reductions + weights)
         xs (if (< (last xs) 1) (concat xs [1]) xs)
         actions (map (fn [start end] {:start start :end end
                                       :sym sym :params params})
                      xs (rest xs))]
     (->Clay actions))))

(defn- shrink-children [offset interval {:keys [children]}]
  (map (fn [m] (-> m
                   (update :start #(+ offset (* interval %)))
                   (update :end   #(+ offset (* interval %)))))
       children))

(defn concat-clay [& clays]
  (let [length (count clays)
        interval (/ 1 length)
        children (->>
                  clays
                  (map-indexed (fn [index cray] (shrink-children (* interval index) interval cray)))
                  (apply concat))]
    (->Clay children)))

(defn merge-clay [& clays]
  (->Clay (->> clays
               flatten
               (map :children)
               (apply concat))))

(defn- update-clay-func [param-key]
  (if (= :sym param-key)
    (fn [child v]
      (if v (assoc child :sym v)
          child))
    (fn [child v]
      (if v (assoc-in child [:params param-key] v)
          child))))

(defn assign [{:keys [cycle?]} param-key vals clay]
  (let [vals (if (coll? vals) vals [vals])
        vals (if cycle? (cycle vals) (concat vals (repeatedly (fn [] nil))))]
    (update clay :children (fn [children] (map (update-clay-func param-key) children vals)))))

(defn shift [amount clay]
  (update clay :children (partial map (fn [c] (-> c
                                                   (update :start #(+ % amount))
                                                   (update :end #(+ % amount)))))))

(defn- convert-to-action
  [{:keys [symbol-table]} dur {:keys [start end sym params]}]
  (let [dur-in-sec (* dur v/*tick-interval-sec* (- end start))
        func-provider (symbol-table sym)
        func (func-provider dur-in-sec params)]
    (m/->Action (* dur start) func)))

(defn burn
  [context dur {:keys [children]}]
  (m/->Queue dur (map (partial convert-to-action context dur) children)))






(ns llll.clay.clay-dsl-parser
  (:require [instaparse.core :as insta]))

(def ^:private token-parser
  (insta/parser
   "token = (sym modifier* | operator | modifier+ | value)
    sym = named-sym | no-name-sym
    named-sym = ':' #'[a-zA-Z0-9-]+'
    no-name-sym = ':'

    operator = #'[!$&+=><?_*|]+'

    modifier = operator value?
    value = number | identifier
    number = #'[+-]?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?'
    identifier = #'[a-zA-Z][a-zA-Z0-9-]*'"))

(defn convert-value-token [[_ [typ v]]]
  {:type :operand
   :value (case typ
            :number (read-string v)
            :identifier (symbol v))
   :as-value true})

(defn convert-operator-token [[_ op]]
  {:type :operator :value (symbol op)})

(defmulti convert-token first)

(defmethod convert-token :sym [[_ [_ _ sym-name] & modifiers]]
  (cons {:type :operand :value (keyword sym-name)
         :as-sym true}
        (map convert-token modifiers)))

(defmethod convert-token :modifier [[_ op value]]
  [(convert-operator-token op)
   (convert-value-token value)])

(defmethod convert-token :value [x]
  [(convert-value-token x)])

(defmethod convert-token :operator [x]
  [(convert-operator-token x)])

(defn- split-token [token]
  (cond
    (number? token) [{:type :operand :value token :as-value true}]

    (or (keyword? token)
        (symbol? token))
    (->> token str token-parser rest (mapcat convert-token))

    (vector? token)
    [{:type :operand :value token :as-sym true}]

    (list? token)
    [{:type :operand :value token :as-sym true :as-value true}]

    true
    (throw (RuntimeException. (str "not acceptable: " token)))))

(defn- ->Modifier [operator value]
  {:operator operator :value value})

(defn- ->Node
  ([sym] {:sym sym :modifiers []})
  ([sym modifiers] {:sym sym :modifiers modifiers}))

(defn- ->Parent
  ([sym] {:children sym :modifiers []})
  ([sym modifiers] {:children sym :modifiers modifiers}))


(declare parse)

(defmulti token->node (fn [acc [prev-token cur-token]]
                        [(:type prev-token) (:type cur-token)]))

(defn- convert-token-as-sym [{:keys [type value as-sym]}]
  (assert (= type :operand))
  (assert as-sym)
  (cond
    (list? value) (->Node value)
    (keyword? value) (->Node value)
    (vector? value) (->Parent (parse value))
    (nil? value) (->Node nil)
    true (throw (RuntimeException. (str "not valid sym: " value)))))

(defn- append-non-nil [xs x]
  (assert (vector? xs))
  (if x (conj xs x) xs))

(defmethod token->node [:operand :operand] [{:keys [result node]} [_ token]]
  (assert (:as-sym token))
  {:result (append-non-nil result node)
   :node (convert-token-as-sym token)})

(defmethod token->node [:operand :operator] [acc [_ token]]
  (assoc acc :op (:value token)))

(defmethod token->node [:operator :operand] [{:keys [result node op] :as acc} [_ token]]
  (assert (:as-value token))
  {:result result
   :node (update node :modifiers #(conj % (->Modifier op (:value token))))})

(defn parse [xs]
  (let [xs (mapcat split-token xs)
        [head & tail] xs
        head-node (convert-token-as-sym head)
        zipped (map (fn [x y] [x y]) xs tail)
        {:keys [result node]} (reduce token->node {:result [] :node head-node} zipped)]
    (append-non-nil result node)))

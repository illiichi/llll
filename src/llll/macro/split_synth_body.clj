(ns llll.macro.split-synth-body
  (:require [clojure.walk :as w]
            [llll.engine.model :as m]
            [llll.sc.sound-control :as snd]
            [overtone.sc.ugens :as ot-u]))


(def ^:private extraction-client-eval
  {:field-name :arguments
   :target? (fn [exp] (and (list? exp) (= (first exp) '!!)))
   :replace-exp (fn [table sexp]
                  (let [variable-name (str "arg-" (count table))]
                    (.put table (keyword variable-name) (first (rest sexp)))
                    (symbol variable-name)))})

(defn- extraction-as-control-bus [line-key]
  {:field-name :control-bus
   :target? (fn [exp] (and (keyword? exp)
                           (not (= :-gate exp))
                           (= (nth (str exp) 1) \-)))
   :replace-exp (fn [table sexp]
                  (.put table sexp sexp)
                  `(ot-u/in:kr (snd/bus ~line-key ~sexp)))})

(defn- walk-or-replace [replace-func tree]
  (letfn [(keep-walk [form] (w/walk (partial replace-func keep-walk) identity form))]
    (keep-walk tree)))

(defn- find-first [pred xs]
  (->> xs (filter pred) first))

(defn- extract-argments [extractions exp]
  (let [extractions (map #(assoc % :table (java.util.HashMap.)) extractions)
        new-exp (walk-or-replace
                 (fn [keep-walk x]
                   (if-let [{:keys [replace-exp table]} (find-first (fn [{:keys [target?]}] (target? x))
                                                                    extractions)]
                     (replace-exp table x)
                     (keep-walk x)))
                 exp)]
    (->> extractions
         (map (fn [{:keys [field-name table]}] [field-name table]))
         (into {:exp new-exp}))))

(defn- append-fields [{:keys [arguments] :as m}]
  (let [arg-names (vec (sort (keys arguments)))]
    (-> m
        (assoc :arg-names arg-names
               :arg-values (mapv #(.get arguments %) arg-names))
        (dissoc :control-bus))))

(defn- make-section-record
  ( [loop-key synth-exp] (make-section-record loop-key {:dur 1} synth-exp))
  ( [loop-key options synth-exp]
   (let [extract-argments (partial extract-argments [extraction-client-eval
                                                     (extraction-as-control-bus loop-key)])]
     (-> synth-exp
         extract-argments
         append-fields
         (merge options)))))

(defn- section-include? [form]
  (cond (= 'section form) true
        (or (list? form) (vector? form)) (some section-include? form)
        true false))

(defn- make-synth-def-args [xs]
  (->> xs
       (mapcat (fn [x] [(symbol (name x)) 0]))
       vec))

(defn split-synth-body [loop-key form]
  (let [keep-node? (not (section-include? form))
        synth-body (if keep-node? [(list 'section form)] form)
        synths (ref [])]
    (letfn [(inner [keep-walk form]
              (if (and (list? form) (= 'section (first form)))
                (dosync
                 (let [number (inc (count @synths))
                       new-record (-> (apply make-section-record loop-key (rest form))
                                      (assoc :number number))]
                   (commute synths #(conj % {:args-def (make-synth-def-args (:arg-names new-record))
                                             :exp (:exp new-record)
                                             :number (:number new-record)}))
                   (dissoc new-record :exp)))
                (keep-walk form)))]
      (let [func-exp (walk-or-replace inner synth-body)]
        {:keep-node? keep-node?
         :func-exp func-exp
         :synths (deref synths)}))))

(defn convert->Queue [total-in-tick action-maker sections]
  (let [total-duration (reduce + 0 (map :dur sections))]
    (->> sections
         (filter #(not= (:dur %) 0))
         (reduce (fn [[xs current] {:keys [:dur :number :arg-names :arg-values]}]
                   (let [dur-in-tick (* total-in-tick (/ dur total-duration))
                         end (+ current dur-in-tick)
                         args (interleave arg-names arg-values)]
                     [(conj xs (m/->Action current (action-maker number dur-in-tick args)))
                      end]))
                 [[] 0])
         (first)
         (m/->Queue total-in-tick))))

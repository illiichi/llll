(ns llll.engine.resource
  (:require [overtone.at-at :as at]
            [llll.engine.engine :as en]))

(defn ->Resource []
  (atom {:state :empty :content nil}))

(defn swap-with-lock [res switching-func]
  (letfn [(reset-func [x] {:state :filled :content x})
          (lock-func [dur-in-sec x]
            (at/after (* 1000 dur-in-sec) (fn []
                                            (reset! res {:state :filled
                                                         :content x}))
                      en/*my-pool*)
            {:state :switching :content x})]

    (locking res
      (let [{:keys [state content]} @res]
        (case state
          :switching
          (throw (RuntimeException. "switching..."))

          :done
          (throw (RuntimeException. "this resouce is done"))

          (reset! res (switching-func content reset-func lock-func)))))))

(defn swap [res switching-func]
  (swap-with-lock res (fn [content reset lock] (reset (switching-func content)))))

(defn finish [res finalizer]
  (locking res
    (let [{:keys [content]} @res]
      (finalizer content)
      (reset! res {:state :done}))))

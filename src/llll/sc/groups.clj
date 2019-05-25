(ns llll.sc.groups
  (:require [overtone.sc.node :as ot-node]
            [mount.core :refer [defstate]]))

(defstate short-life
  :start (ot-node/group "short life nodes group")
  :stop (ot-node/group-free short-life))

(defstate long-life
  :start (ot-node/group "long life nodes group" :after short-life)
  :stop (ot-node/group-free long-life))


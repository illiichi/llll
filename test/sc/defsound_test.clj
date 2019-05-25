(ns sc.defsound-test
  (:require [llll.core :as l4]
            [llll.macro.defsound :refer :all]
            [llll.macro.control :refer :all]))

(comment
 (use 'overtone.core)
 (connect-external-server "localhost" 57110)
 (kill-server)
 (l4/finish)

 (l4/initialize {})

 (l4/control :test :vol {:dur 16 :to 1})
 (defsound test
   {:swap-option {:fade-in-dur 8
                  :switch-dur 8
                  :fade-out-dur 8}}
   (sin-osc 880))
 (l4/control :test :vol {:dur 8 :to 1})
 (l4/stop :test))

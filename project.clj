(defproject llll "0.1.2"
  :description "A sound system which aims to explore all possible sound by computer with live coding"
  :url "http://github.com/illi-ichi/llll"
  :license {:name "MIT License"
            :url "http://github.com/illi-ichi/llll/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [overtone            "0.10.6"]
                 [instaparse "1.4.9"]
                 [mount "0.1.12"]]
  ;; add per WARNING: JVM argument TieredStopAtLevel=1 is active...
  :jvm-opts ^:replace []
  :repositories [["clojars" {:url "https://clojars.org/repo"
                             :username [:env/clojars_username :gpg]
                             :password [:env/clojars_password :gpg]}]]
  :deploy-repositories  [["releases" :clojars]
                         ["snapshots" :clojars]]
  :signing {:gpg-key "868C4511"})


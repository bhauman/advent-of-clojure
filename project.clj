(defproject advent "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [digest "1.4.4"]
                 [medley "0.8.4"]
                 [criterium "0.4.4"]
                 [clj-time "0.15.1"]
                 [net.mikera/core.matrix "0.57.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/core.match "0.2.2"]]

  :profiles { :repl { :repl-options {:init (set! *print-length* 500)}} })

(ns advent.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.core.match :refer [match]]
   [clojure.pprint :as p]
   [clojure.set :refer [union]]
   [clojure.math.combinatorics :as combo]
   [clojure.walk :refer [prewalk postwalk]]
   [digest :refer [md5]]))

;; workarea


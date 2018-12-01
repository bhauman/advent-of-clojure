(ns advent-2015.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.core.match :refer [match]]
   [clojure.pprint :as p]
   [clojure.set :refer [union intersection difference]]
   [clojure.math.combinatorics :as combo]
   [clojure.walk :refer [prewalk postwalk]]
   [digest :refer [md5]]))











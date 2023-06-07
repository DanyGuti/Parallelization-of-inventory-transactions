(ns sp-p3.core-test
  (:require [clojure.test :refer :all]
            [sp-p3.core :refer :all :as core]))

(deftest testMainCore
  (testing "Multiple threads: ")
  (core/repeat-main core/-main 5))


(run-tests)
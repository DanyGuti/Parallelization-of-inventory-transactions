(ns sp-p3.core-test
  (:require [clojure.test :refer :all]
            [sp-p3.core :refer :all :as core]
            [sp-p3.createTxt :as userInput]))

; (deftest testMainCore
;  (testing "Multiple threads: ")
;  (core/repeat-main core/-main 1))

(def number
  (core/generate-random-number))

(deftest testMapTransactions
  (testing "One threads: ")
  (let [n 900
        txt (userInput/generate-n-txt n)
        out (userInput/generate-output-txt n)
        n-transactions 40
        movements (doall (repeatedly n #(core/generate-transactions n-transactions)))
        ;; Make the list of haçsh maps with output paths 
        ;; ({inventory1.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))}
        ;;  {inventory2.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))})
        inventories-hash (doall (core/generate-inventories-hash movements core/paths-inv (take n core/paths-output)))
        non-parallel-results (time (print "Time with map with " n " inventories and " n-transactions " transactions" (dorun (core/transactions-n-map inventories-hash))))]))

(deftest testPmapTransactions
  (testing "Multiple threads: ")
  (let [n 900
        txt (userInput/generate-n-txt n)
        out (userInput/generate-output-txt n)
        n-transactions 40
        movements (doall (repeatedly n #(core/generate-transactions n-transactions)))
        ;; Make the list of haçsh maps with output paths 
        ;; ({inventory1.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))}
        ;;  {inventory2.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))})
        inventories-hash (doall (core/generate-inventories-hash movements core/paths-inv (take n core/paths-output)))
        parallel-results (time (print "Time with pmap with " n " inventories and " n-transactions " transactions" (dorun (core/transactions-n inventories-hash n))))]))



(run-tests)
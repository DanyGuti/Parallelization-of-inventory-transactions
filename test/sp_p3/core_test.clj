;  (core/repeat-main core/-main 1))
(ns sp-p3.core-test
  (:require [clojure.test :refer :all]
            [sp-p3.core :refer :all :as core]
            [sp-p3.createTxt :as userInput]))

; (deftest testMainCore
;  (testing "Multiple threads: ")

;(def number
;  (core/generate-random-number))

(defn inventories []
  (core/generate-random-number))

(defn  transactions []
  (inc (rand-int 200)))

(defn repeat-test [n]
  (dorun (take n (repeatedly #(run-tests)))))

(defn testMapTransactions [n transactions]
  (let [txt (userInput/generate-n-txt n)
        out (userInput/generate-output-txt n)
        movements (doall (repeatedly n #(core/generate-transactions transactions)))
        ;; Make the list of haçsh maps with output paths 
        ;; ({inventory1.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))}
        ;;  {inventory2.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))})
        inventories-hash (doall (core/generate-inventories-hash movements core/paths-inv (take n core/paths-output)))
        non-parallel-results (core/transactions-n-map inventories-hash)]
    (time (print "Time to write output files with map: "
                 (println "With: " n " inventories and: " transactions " transactions " (dorun (core/process-output-write-map non-parallel-results))))))
  (Thread/sleep 10000))

(defn testPmapTransactions [n transactions]
  (testing "Multiple threads: ")
  (let [txt (userInput/generate-n-txt n)
        out (userInput/generate-output-txt n)
        movements (doall (repeatedly n #(core/generate-transactions transactions)))
        ;; Make the list of haçsh maps with output paths 
        ;; ({inventory1.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))}
        ;;  {inventory2.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))})
        inventories-hash (doall (core/generate-inventories-hash movements core/paths-inv (take n core/paths-output)))
        calculate-partitions (cond
                               (< n 100) 10
                               (and (> n 100) (< n 1000)) (/ n 10)
                               (and (> n 1000) (<= n 10000)) 1000
                               :else 5000)
        parallel-results (core/transactions-n inventories-hash calculate-partitions)]
    (time (print "Time to write output files with pmap: "
                 (println "With: " n " inventories and: " transactions " transactions " (dorun (core/process-output-write-pmap parallel-results calculate-partitions)))))))

(deftest testThreads
  (testing "One thread and multiple threads ")
  (let [n (inventories)
        transactions (transactions)]
    (testMapTransactions n transactions)
    (testPmapTransactions n transactions)))


(repeat-test 3)
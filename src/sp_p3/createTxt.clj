(ns sp-p3.createTxt (:require [clojure.string :as str]
                              [clojure.java.io :as io]))

;; Generate product keys from a starting point to an end point
(defn generate-products [start end]
  (map
   (fn [num] (str (char num))) (range (int start) (inc (int end)))))


;; Create a random inventory ready to be written in .txt
;; Returns a list of all products, price, index and quantity
(defn create-random-inv [products]
  (map (fn [product]
         (let [random-rows
               (map (fn [index]
                      (list (str product index)
                            (str  (rand-int 1000))
                            (str index)
                            (str (rand-int 1000))))
                    (range 1 6))
               create-rows (map (fn [sublst] (str/join " " sublst)) random-rows)]
           create-rows)) products))

;; Create n number of txt, with the random inventory creation in
;; price and quantity
(defn generate-n-txt [n]
  (let [indexes (range 1 (+ n 1))
        file-paths (map (fn [x] (str "inventory" x ".txt")) indexes)
        folder-path (str (System/getProperty "user.dir") "/resources/")]
    (doseq [file-path file-paths]
      (with-open [writer (io/writer (str folder-path file-path))]
        (.write writer (str/join "\n" (flatten (create-random-inv (generate-products \A \G)))))))))

;; Create n number of txt, with the random inventory creation in
;; price and quantity
(defn generate-output-txt [n]
  (let [indexes (range 1 (+ n 1))
        file-paths (map (fn [x] (str "output" x ".txt")) indexes)
        folder-path (str (System/getProperty "user.dir") "/outputs/")]
    (doseq [file-path file-paths]
      (spit (str folder-path file-path) ""))))

(defn generate-files []
  (println "Escribe cuantos inventarios tienes: ")
  (let [input (Integer/parseInt (read-line))]
    (if (integer? input)
      (do
        (generate-n-txt input)
        (generate-output-txt input)
        input)
      (print "No es un número!!"))))



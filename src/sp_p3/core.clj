(ns sp-p3.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [sp-p3.createTxt :as userInput]))

(load "createTxt")

(def left-regex  #"^[A-Z]1$") ; Out of bounds to the left when match
(def right-regex  #"^[A-Z]5$") ; Out of bounds to the right when match

;; Regex for matching wheter an inventoryn.txt
;; or an outputn.txt
(def inventory-regex #"^(inventory|output)\d+\.txt$")

(def extract-file-regex #"(inventory|output)\d+\.txt$")

;; Directory for the txt inventories
(def resources-path (io/file (str (System/getProperty "user.dir") "/resources")))

;; Make an instance of an object to lock transactions
(def lock (Object.))

;; Directory for the txt outputs
(def outputs-path (io/file (str (System/getProperty "user.dir") "/outputs")))

;; Count the number of inventories in /resources
(defn count-files [resource-path]
  (count (file-seq resource-path)))

;; Create num-file global var
(def num-file (count-files resources-path))

;; Create num-file global var
(def num-file-output (count-files outputs-path))

;; Make exceptions for txt extension
(defn different-extension? [file-path inv-regex]
  (not (re-matches inv-regex file-path)))

;; Make list of txt extensions
;; only if the extension is valid .txt
(defn file-paths [n name]
  (try (let [indexes (range 1 n)
             file-paths (remove nil? (map (fn [x] (str name x ".txt")) indexes))
             diff-paths (filter
                         (fn [file] (different-extension? file inventory-regex)) file-paths)]
         (if (seq diff-paths)
           (throw (Exception. "Files with different extensions found."))
           file-paths))
       (catch Exception e
         (println e))))

;; Define list of inventory paths
(def list-paths (doall (remove nil? (file-paths num-file "inventory"))))

;; Define list of output paths
(def list-paths-output (doall (remove nil? (file-paths num-file-output "output"))))

;; Make an inventory based on reading the txt file
(defn make-inventory [data]
  (let [letters (distinct (map
                           (fn [letter] (str (subs (first letter) 0 1))) data))]
    (reverse (reduce (fn [row letter]
                       (conj row (apply concat [(str letter) (filter (fn [x] (= (subs (first x) 0 1) letter)) data)]))) () letters))))

;; When matching, generates true if match found
;; else, return false
(defn matches-regex [regex window]
  (if (= (re-matches regex window) nil)
    false
    true))

;; Iterate to the check if out of bounds, return true if out of bounds
(defn out-of-bounds [left-right-col window symbol]
  (cond
    (nil? left-right-col) false
    (and (= symbol -1) (matches-regex left-regex (first window))) true
    (and (= symbol 1) (matches-regex right-regex (first window))) true
    :else (out-of-bounds (next left-right-col) window symbol)))

;; Generate 'keys' of data structure
(def generate-row
  (fn [inv]
    (map (fn [key] (str (first key))) inv)))

;; Generate the values associated to the key 'letter'
;; being passed
(def generate-rows
  (fn [inv letter]
    (let [rows (filter (fn [key] (= (str (first key)) letter)) inv)] (first (map next rows)))))

;; Generate the sum of the inventory
(def sum-inv-price
  (fn [lists]
    (apply + (map (fn [sublsts] (* (Integer/parseInt (first (next sublsts))) (Integer/parseInt (first (next (next (next sublsts))))))) lists))))

;; Generate a list of lists to sum inventory
(def generate-cost
  (fn [file-path]
    (let [file-contents (slurp file-path)
          lines (str/split-lines file-contents)
          lists (map (fn [line]
                       (let [string (str/split line #" ")]
                         (flatten string))) lines)]
      (list (sum-inv-price lists) file-path))))

;; Matches a row and a col based on a letter and index
(def match-row-col
  (fn [row-match index]
    (first (filter (fn [row] (= (Integer/parseInt index) (Integer/parseInt (first (next (next row)))))) row-match))))

;; Increase or decrease ASCII char based on passed char
(defn move-char [c symbol]
  (let [char-int (int (first c))]
    (str (char (symbol char-int 1)))))

;; Generate the right col
(def get-col-right
  (fn [inv]
    (map last (map next inv))))

;; Generate the left col
(def get-col-left
  (fn [inv]
    (map first (map next inv))))

;; Minimum recursive calls
;; ((3 ("A1" "100" "1" "0")) (1 ("A1" "100" "1" "0"))) --> (3 ("A1" "100" "1" "0")
(defn min-recursive-steps [lst]
  ;; (println "min-recursive-steps input: " lst) ; Add this line
  (apply min-key first lst))


;; Check for substrings
;; -1 substr(0, 1) --> A
;; 0 substr(1, 2)  --> 1
;; 1 substr(0, 2)  --> A1
(def get-letter-index-window
  (fn [window flag]
    (cond
      (= flag -1) (subs (first window) 0 1)
      (= flag 0) (subs (first window) 1 2)
      (= flag 1) (subs (first window) 0 2))))

;; Check for substrings
;; -1 substr(0, 1) --> A
;; 1 substr(0, 2)  --> A1
(def get-product-letter-index
  (fn [product flag]
    (cond
      (= flag -1) (subs product 0 1)
      (= flag 1) (subs product 0 2))))

;; Generate letter when rows and window passed
(def get-letter-match
  (fn [row window]
    (first (filter (fn [rows] (= rows (subs (first window) 0 1))) row))))

;; Look for same indexes in inventory
;; Return window at position
(defn look-inventory [row index window]
  (cond
    (nil? window) nil
    (= (Integer/parseInt (first (next (next (first window))))) index) (first window)
    :else (look-inventory row index (next window))))

;; Move to the left or right based on passed flag
;; 0 go to the left
;; 1 go to the right
(defn move-left-right [inventory window flag]
  (cond
    (nil? inventory) window
    (= flag 0) (look-inventory (get-letter-match (generate-row inventory) window) (- (Integer/parseInt (first (next (next window)))) 1)
                               (generate-rows inventory (get-letter-match (generate-row inventory) window)))
    (= flag 1) (look-inventory (get-letter-match (generate-row inventory) window) (+ (Integer/parseInt (first (next (next window)))) 1)
                               (generate-rows inventory (get-letter-match (generate-row inventory) window)))
    :else (move-left-right (next inventory) window flag)))

;; Move up or down based on passed symbol
;; - go up
;; + go down
(defn move-up-down [inventory window symbol]
  (cond
    (nil? inventory) window
    :else (let [result (get-letter-match (generate-row inventory) window)]
            (cond
              (and (= (subs result 0 1) "A") (= symbol -)) (match-row-col (generate-rows inventory "G") (first (next (next window))))
              (and (= (subs result 0 1) "G") (= symbol +)) (match-row-col (generate-rows inventory "A") (first (next (next window))))
              :else (match-row-col (generate-rows inventory (str (move-char (get-letter-match (generate-row inventory) window) symbol))) (first (next (next window))))))))

;; Receive inventory and window
;; Calls move-up-down with - to move one cell UP
(defn U [inventory window]
  (cond
    (nil? inventory) nil
    :else (move-up-down inventory window -)))

;; Receive inventory and window
;; Calls move-up-down with + to move one cell DOWN
(defn D [inventory window]
  (cond
    (nil? inventory) nil
    :else (move-up-down inventory window +)))

;; Receive inventory and window
;; Calls move-left-right if not out of bounds with 0 to move left
(defn L [inventory window]
  (cond
    (nil? inventory) nil
    (out-of-bounds (get-col-left inventory) window -1) nil
    :else (move-left-right inventory window 0)))

;; Receive inventory and window
;; Calls move-left-right if not out of bounds with 0 to move left
(defn R [inventory window]
  (cond
    (nil? inventory) nil
    (out-of-bounds (get-col-right inventory) window 1) nil
    :else (move-left-right inventory window 1)))

;; Evaluate till the product is found to the left or right
;; Return the steps to get there
;; -1 move left
;; 1 move right
(defn min-check-cols-left-right [inventory window product movement]
  (cond
    (nil? window) (list 0 window)
    (= (get-letter-index-window window 1) (get-product-letter-index product 1)) (list 0 window)
    (= movement -1) (let [result (min-check-cols-left-right inventory (L inventory window) product movement)] (cons (+ 1 (first result)) (next result)))
    :else (let [result (min-check-cols-left-right inventory (R inventory window) product movement)] (cons (+ 1 (first result)) (next result)))))

;; Evaluate till the product is found to the right
;; Return the steps to get there
;; returns left or right steps based on the results
(defn min-check-cols [inventory window product]
  (cond
    (= (get-letter-index-window window 1) (get-product-letter-index product 1)) (list 0 window)
    (= (get-letter-index-window window 0) "1") (let [result-right (min-check-cols-left-right inventory (R inventory window) product 1)]
                                                 (cons (+ 1 (first result-right)) (next result-right)))
    (= (get-letter-index-window window 0) "5") (let [result-left (min-check-cols-left-right inventory (L inventory window) product -1)]
                                                 (cons (+ 1 (first result-left)) (next result-left)))
    :else (let [result-right (min-check-cols-left-right inventory (R inventory window) product 1)
                result-left (min-check-cols-left-right inventory (L inventory window) product -1)]
            (cond
              (nil? (first (next result-right))) (cons (+ 1 (first result-left)) (next result-left))
              :else (cons (+ 1 (first result-right)) (next result-right))))))

;; Evaluate till the row is found upwards
;; return the sum of the steps and the state at the window
(defn min-to-row-up-down [letter inventory window product symbol]
  (cond
    (= (get-letter-index-window window -1) (get-product-letter-index product -1)) (min-check-cols inventory window product)
    (= symbol -) (let [result (min-to-row-up-down letter inventory (U inventory window) product symbol)] (cons (+ 1 (first result)) (next result)))
    :else
    (let [result (min-to-row-up-down letter inventory (D inventory window) product symbol)] (cons (+ 1 (first result)) (next result)))))

;; Modify the file
(defn modify-file [file-path window]
  (let [file (io/file file-path)
        file-contents (slurp file-path)
        lines (str/split-lines file-contents)
        lists (map (fn [line]
                     (let [string (str/split line #" ")]
                       (flatten string))) lines)
        new-modified (map (fn [string]
                            (if (= (first string) (first window))
                              (str/join " " window)
                              (str/join " " string)))
                          lists)]
    (with-open [writer (io/writer file)]
      (doseq [line new-modified]
        (.write writer (str line "\n"))))))

;; Retire product with the min possible steps to get
;; there from a starting position
;; (first (next (next (next (first (next lista))))))
(defn add-retire-product [product quantity list-steps-window symbol file-path]
  (cond
    (nil? product) (let [result (symbol (Integer/valueOf (first (next (next (next list-steps-window))))) quantity)]
                     (cond
                       (< (Integer/valueOf result) 0) (let [modified-window (list (first list-steps-window) (str (next (first list-steps-window))) (first (next (next list-steps-window))) (str 0))]
                                                        (str " Rellena este producto! 0 del producto" (first list-steps-window)  (modify-file file-path modified-window)))
                       :else (let [modified-window (list (first list-steps-window) (str (next (first list-steps-window))) (first (next (next list-steps-window))) (str result))]
                               (str " Queda: " result " del producto " product (first list-steps-window) (modify-file file-path modified-window)))))
    (list? (next (first list-steps-window))) (let [result (symbol (Integer/parseInt (first (next (next (next (first (next (min-recursive-steps list-steps-window)))))))) quantity)
                                                   min-steps (first (min-recursive-steps list-steps-window))]
                                               (cond
                                                 (< (Integer/valueOf result) 0) (let [modified-window
                                                                                      (list  (str (first (first (next (first list-steps-window)))))
                                                                                             (str (first (next (first (next (first list-steps-window))))))
                                                                                             (str (first (next (next (first (next (first list-steps-window)))))))
                                                                                             (str 0))]
                                                                                  (str " Queda: 0 del product o" product " RELLENA AL PRODUCTO " product ". Transacción hecha con los sig. pasos: " min-steps (modify-file file-path modified-window)))
                                                 :else (let [modified-window
                                                             (list (str (first (first (next (first list-steps-window)))))
                                                                   (str (first (next (first (next (first list-steps-window))))))
                                                                   (str (first (next (next (first (next (first list-steps-window)))))))
                                                                   (str result))]
                                                         (str " Queda: " result " del producto: " product ". Transacción hecha con los sig pasos: " min-steps (modify-file file-path modified-window)))))
    :else (let [result (symbol (Integer/valueOf (first (next (next (next list-steps-window))))) quantity)]
            (cond
              (< (Integer/valueOf result) 0) (let [modified-window (list (first list-steps-window) (str (first (next list-steps-window))) (first (next (next list-steps-window))) (str 0))]
                                               (str " Rellena este producto! 0 del producto" (first list-steps-window)  (modify-file file-path modified-window)))
              :else (let [modified-window (list (first list-steps-window) (str (first (next list-steps-window))) (first (next (next list-steps-window))) (str result))]
                      (str " Queda: " result " del producto " product (first list-steps-window) (modify-file file-path modified-window)))))))

;; Min steps to get to a product
;; First go up, then down.
;; For each of those go left and right till out of bounds
;; If out of bounds, just return the other list with the counter
(defn min-steps-to-product [rows product quantity inventory window symbol file-path]
  (cond
    (nil? inventory) nil
    (nil? product) (add-retire-product nil quantity window symbol file-path)
    (= (subs product 0 2) (subs (first window) 0 2)) (add-retire-product product quantity window symbol file-path)
    (= (first rows) (get-product-letter-index product -1))
    (let [min-up (min-to-row-up-down (get-letter-match (generate-row inventory) window) inventory window product -)
          min-down (min-to-row-up-down (get-letter-match (generate-row inventory) window) inventory window product +)]
      (cond

        (nil? (next min-up)) min-down
        (nil? (next min-down)) min-up
        :else (add-retire-product product quantity (list min-up min-down) symbol file-path)))
    :else (min-steps-to-product (next rows) product quantity inventory window symbol file-path)))

;; Read txt by file path
;; -> file path
;; --> string
(defn read-txt-file [file-path]
  (slurp file-path))

;; -> file path
;; --> List of lists (("A1" "100" "1" "20")("A2" "200" "2" "30") ("A3" "300" "3" "30"))
(defn read-file [file-path]
  (let [file-contents (read-txt-file file-path)
        lines (str/split-lines file-contents)]
    (map (fn [line]
           (let [string (str/split line #" ")]
             (flatten string))) lines)))

;; Get the cost of every inventory
;; --> list with costs
(defn printCosts [n]
  (map (fn [file] (generate-cost (str (System/getProperty "user.dir") "/resources/" file))) (take n list-paths)))

;; Get the paths to every inventory
;; --> list with all inventory files
(def paths-inv
  (map (fn [file] (str (System/getProperty "user.dir") "/resources/" file)) list-paths))

;; Get the paths to every output
;; --> list with all output files
(def paths-output
  (map (fn [file] (str (System/getProperty "user.dir") "/outputs/" file)) list-paths-output))

;; Ask for n inputs
;; --> n as user input
(defn generate-n []
  (let [input (do (println "Escribe el número de transacciones: ") (read-line))]
    (if (integer? (Integer/parseInt input))
      (Integer/parseInt input)
      (print "No es un numero!!"))))


;; Generate random transactions
;; First the product "A"
;; Then a number from 1 to 5
;; Then a quantity
;; Followed by making the sublist: ("A1" "200" "1" "0")
;; Then make a list of random transactions from every transaction possible
;; Make a list of both things, sublist and transaction
;; -> n number of times
;; --> list with window and transactions to be made
(defn generate-transactions [n]
  (let [random-list (repeatedly 1 #(+ (rand-int (- 71 65)) 65))
        products  (map (fn [_]
                         (let [num1 (inc (rand-int 5))
                               num2 (rand-int 1001)
                               sub-list (list (str (char (rand-nth random-list)) num1) (str num2) (str num1) "0")
                               rand-trans (repeatedly n #(rand-nth (list "U" "R" "D" "L" "-" "+")))]
                           (list sub-list rand-trans))) random-list)]
    products))

;; Generate random products for making the add or substract
;; transactions
;; -> n number of times
;; --> product between "A" to "G" n times
(defn generate-prod [n]
  (let [random-list (repeatedly n #(+ (rand-int (- 71 65)) 65))
        product (map (fn [x] (list (str (char x) (inc (rand-int 5))) (rand-int 1000))) random-list)]
    product))

;; Make a list of hash maps with the txt to read
;; the random movements and also the output files
;; -> products, paths, output-files
;; --> list of hash maps
(defn generate-inventories-hash [products paths output-files]
  (let [inventory (hash-map)
        keys (map (fn [x] (str x)) paths)
        product products
        vals (map (fn [val file] (conj val (list file))) product output-files)]
    (map (fn [x y] (assoc inventory x y)) keys vals)))

;; Parse every transaction, descent recursive based on the window
;; -> window, transaction-list, key, output-file
;; --> list of strings
(defn make-transactions-n [window transaction-list key output-file]
  (cond
    (nil? transaction-list) nil
    :else
    (let [type-transaction (first transaction-list)]
      (cond
        (or (or (or (= type-transaction "U")
                    (= type-transaction "D"))
                (= type-transaction "R"))
            (= type-transaction "L"))
        (let [new-window
              (cond
                (= type-transaction "U") (U (make-inventory (read-file (first key))) window)
                (= type-transaction "D") (D (make-inventory (read-file (first key))) window)
                (= type-transaction "R") (R (make-inventory (read-file (first key))) window)
                (= type-transaction "L") (L (make-inventory (read-file (first key))) window)
                :else window)]
          (cond
            (nil? new-window) (let [result (make-transactions-n window (next transaction-list) key output-file)]
                                (cons (list (str "Desde ventana " (apply list (doall window)) " con movimiento: " (first transaction-list) " comando no válido, error!!! ") (list output-file))  result))
            :else (let [result (make-transactions-n new-window (next transaction-list) key output-file)]
                    (cons (list (str "Desde ventana " (apply list (doall window)) " con movimiento: " (first transaction-list) " se llegó a: " (apply list (doall new-window))) (list output-file)) result))))
        :else
        (let [product (generate-prod 1)]
          (if (= type-transaction "-")
            (let [result (make-transactions-n window (next transaction-list) key output-file)]
              (cons (list (str  "Movimiento desde: " (apply list (doall window)) (locking lock (min-steps-to-product (generate-row (make-inventory (read-file (first key)))) (first (first product)) (first (next (first product))) (make-inventory (read-file (first key))) window - (first key)))) (list output-file)) result))
            (let [result (make-transactions-n window (next transaction-list) key output-file)]
              (cons (list (str "Movimiento desde: " (apply list (doall window)) (locking lock (min-steps-to-product (generate-row (make-inventory (read-file (first key)))) (first (first product)) (first (next (first product))) (make-inventory (read-file (first key))) window + (first key)))) (list output-file)) result))))))))


;; Generate transactions based on the list of hash maps inventories
;; make parallel reading files making faster execution, instead of stacking every 
;; hash map
(defn transactions-n [inventories n]
  (let [filtered-inventories (remove nil? inventories)
        transaction-results (pmap (fn [partition]
                                    (pmap (fn [vals]
                                            (let [key (map key vals)
                                                  output (first (first (first (map val vals))))
                                                  window (first (first (next (first (map val vals)))))
                                                  transaction-list (first (next (first (next (first (map val vals))))))]
                                              (make-transactions-n window transaction-list key output)))
                                          partition))
                                  (partition-all 1000 filtered-inventories))
        flattened-results (apply concat transaction-results)]
    flattened-results))

;; Generate transactions based on the list of hash maps inventories
;; make parallel reading files making faster execution, instead of stacking every 
;; hash map
(defn transactions-n-map [inventories]
  (let [filtered-inventories (remove nil? inventories)
        transaction-results (map (fn [vals]
                                   (let [key (map key vals)
                                         output (first (first (first (map val vals))))
                                         window (first (first (next (first (map val vals)))))
                                         transaction-list (first (next (first (next (first (map val vals))))))]
                                     (make-transactions-n window transaction-list key output)))
                                 filtered-inventories)]
    transaction-results))

;; Process every result, use a thread for every output file
;; to make the parallel writing
(defn process-output-write [results]
  (let [output-files (map (fn [sublist] (first (next (first sublist)))) results)
        transaction-results (map (fn [result] (map first result)) results)
        result (map (fn [x y] (concat x y)) transaction-results output-files)]
    (doall
     (pmap (fn [partition]
             (doseq [sublist partition]
               (let [file (last sublist)
                     eval-sublist (drop-last (doall sublist))
                     output-string (str/join "\n" eval-sublist)]
                 (with-open [writer (io/writer file)]
                   (.write writer output-string)))))
           (partition-all 500 result)))))

(defn generate-random-number []
  (inc (rand-int 1000)))

(defn -main [& args]
  (let [n (generate-random-number)
        txt (userInput/generate-n-txt n)
        out (userInput/generate-output-txt n)
        n-transactions (inc (rand-int 50))
        movements (doall (repeatedly n #(generate-transactions n-transactions)))
        ;; Make the list of haçsh maps with output paths 
        ;; ({inventory1.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))}
        ;;  {inventory2.txt (("output1.txt")("A1" "200" "1" "0") ("L" "R" "-" "+" "-"))})
        inventories-hash (doall (generate-inventories-hash movements paths-inv (take n paths-output)))
        parallel-results  (transactions-n inventories-hash n)
        percentage (Math/ceil (* n 0.10))]
        ;;non-parallel-results (time (print "Time with map: " (dorun (transactions-n-map inventories-hash))))]
    (process-output-write parallel-results)
    (let [costs (map (fn [costs] (first costs)) (printCosts n))
          sorted-costs (reverse (sort-by first (printCosts n)))
          paths (map second (take percentage sorted-costs))]
      (println "Con: " n " Inventarios")
      (println "Con: " n-transactions " Transacciones")
      (println "El valor total de tu almacén después de las transacciones es:" (apply + costs))
      (println "El 10% de los inventarios con más producto son: ")
      (let [matcher (map (fn [path] (re-matcher extract-file-regex path)) paths)
            inventories (map (fn [file] (first (re-find file))) matcher)]
        (println inventories))))
  args)

(defn repeat-main [main n]
  (cond
    (= n 0) nil
    (> n 0) (do (time (dorun (main)))
                (repeat-main main (- n 1)))))

(ns cs253.project)
; Initialization

   
  
(defn rand-subtree
  "This function generates a random subtree."
  [terminal-set function-set]
  (let [rand-item (rand-nth (rand-nth '(terminal-set function-set)))]
    (if (fn? rand-item )
      (list (rand-item (rand-nth terminal-set) (rand-nth terminal-set)))
      rand-item)))

(defn random-program
  ""
  [terminal function]
  (let [item (rand-nth (rand-nth (list terminal function)))]
    (if #(contains? function %) item)
      (list item (random-program terminal function) (random-program terminal function))
      item))

(random-program '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 x) '(+ - *))

(list (rand-nth (rand-nth (list '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 x) '(+ - *))))
      3 5)
(rand-nth ('(2 3 4  x) '(+ - *)))

(rand-subtree '(2 3 4  x) '(+ - *))

(defn example-nest
  []
  ""
  (let [a (range)]
    (while #(<= 10 %) a
      (list a example-nest))))

(example-nest )

(defn full-tree
  "This function creates a random full-tree"
  [terminal-set function-set max-depth]
  (list (rand-nth function-set)
        (if (= 0 max-depth)
          (rand-nth terminal-set)
          (full-tree terminal-set function-set (dec max-depth)))
        (if (= 0 max-depth)
          (rand-nth terminal-set)
          (full-tree terminal-set function-set (dec max-depth)))))

(defn grow-tree
  "This function creates a random grow-tree"
  [terminal-set function-set max-depth]
  
  (let [item (rand-nth (concat function-set terminal-set) )]
          (if (some #(= item %) terminal-set)
            item
         (list item
               (if (or (= 0 max-depth) (some #(= item %) terminal-set))
                 (rand-nth terminal-set)
                 (grow-tree terminal-set function-set (dec max-depth)))
                      
               (if (or (= 0 max-depth) (some #(= item %) terminal-set))
                 (rand-nth terminal-set)
                 (grow-tree terminal-set function-set (dec max-depth)))
         ))))
  
(grow-tree '(2 3 4 5) '(+ * -) 2)
  
(grow-tree '(2 3 4 5) '(+ * -) 3)
(full-tree '(2 3 4 5) '(+ * -) 3)

(defn make-population
  "Makes a population of n number of program-trees of a random max-depth in range [2,6] 
using terminal-set and function-set"
  [n terminal-set function-set]
  (list (take (* 0.5 n)(repeat (full-tree terminal-set function-set (rand-nth (range 2 6)))))
        (take (* 0.5 n)(repeat (grow-tree terminal-set function-set (rand-nth (range 2 6)))))))


(make-population 4 '(2 3 4 5) '(+ * -))

        

(defn rand-program
  "This program generates a random program
  using a max-depth and a primitive set."
  [max-depth terminal-set function-set]
  )


(defn gen-population
  "This program generates a population of
   randomly generated programs."
  [num-pop max-depth terminal-set function-set]
  )

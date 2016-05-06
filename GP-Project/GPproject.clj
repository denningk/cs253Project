(ns cs253.GPproject)


; Initialization

(defn rand-frontier-tree
  "This function generates a random subtree with one function
  and its arguments."
  [terminal-set function-set]
  (list (rand-nth function-set) (rand-nth terminal-set) (rand-nth terminal-set)))

(rand-frontier-tree '(2 3 4 x) '(+ - *))

(defn rand-terminal
  "This function returns a random terminal from the terminal set."
  [terminal-set]
  (list (rand-nth terminal-set)))
  
(rand-terminal '(2 3 4 x))

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


(full-tree '(2 3 5 x) '(+ - *) 1)


(defn grow-tree
  "This function generates a random tree."
  [terminal-set function-set]
  (loop [tree '()]
    (if (> (rand-int 0 100) 60)
      )))

(some #(= '+ %) '(+ - *))
(grow-tree '(2 3 4 x) '(+ - *))
  

(defn rand-program
  "This program generates a random program
  using a max-depth and a primitive set."
  [max-depth terminal-set function-set]
  )


(defn gen-population
  "This program generates a population of
   randomly generated programs."
  [num-pop max-depth terminal-set function-set]
  (loop [pop []
         num num-pop]
    (if (= num 0)
      pop
      (recur (conj pop (full-tree terminal-set function-set max-depth))
             (dec num)))))

(gen-population 5 3 '(2 3 4 x) '(+ - *))

(ns cs253.GPproject)


; Initialization

   
  
(defn rand-tree
  "This function generates a random subtree."
  [terminal-set function-set]
  (loop [tree '()]
    (let [rand-item (rand-nth (concat terminal-set function-set))]
      (println rand-item)
      (if (some #(= rand-item %) function-set)
        (recur (conj tree rand-item))
        (conj tree rand-item)))))

(some #(= '+ %) '(+ - *))
(rand-tree '(2 3 4 x) '(+ - *))
  

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

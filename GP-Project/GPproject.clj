(ns cs253.GPproject)


; Initialization

   
  
(defn rand-subtree
  "This function generates a random subtree."
  [terminal-set function-set]
  (let [rand-item (rand-nth (rand-nth '(terminal-set function-set)))]
    (if (fn? rand-item )
      '(rand-item (rand-nth terminal-set) (rand-nth terminal-set))
      '(rand-item))))

(rand-nth ('(2 3 4  x) '(+ - *)))

(rand-subtree '(2 3 4  x) '(+ - *))
  

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

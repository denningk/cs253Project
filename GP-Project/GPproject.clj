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
        (if (= 1 max-depth)
          (rand-nth terminal-set)
          (full-tree terminal-set function-set (dec max-depth)))
        (if (= 1 max-depth)
          (rand-nth terminal-set)
          (full-tree terminal-set function-set (dec max-depth)))))


(full-tree '(2 3 5 x) '(+ - *) 5)



(defn grow-tree
  "This function creates a random grow-tree"
  [terminal-set function-set max-depth]
  
  (let [item (rand-nth (concat function-set terminal-set) )]
    (if (some #(= item %) terminal-set)
      item
      (list item
            (if (or (= 1 max-depth) (some #(= item %) terminal-set))
              (rand-nth terminal-set)
              (grow-tree terminal-set function-set (dec max-depth)))
            (if (or (= 1 max-depth) (some #(= item %) terminal-set))
              (rand-nth terminal-set)
              (grow-tree terminal-set function-set (dec max-depth)))
            ))))
  
(grow-tree '(2 3 4 5) '(+ * -) 2)


(grow-tree '(2 3 4 x) '(+ - *) 3)
  

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


(defn make-population
  "Makes a population of n number of program-trees of a random max-depth in range [2,6] 
using terminal-set and function-set"
  [pop-num terminal-set function-set max-depth]
  (list (take (* 0.5 pop-num)(repeat (full-tree terminal-set function-set max-depth)))
        (take (* 0.5 pop-num)(repeat (grow-tree terminal-set function-set max-depth)))))



(make-population 5 '(2 3 4 5) '(+ * -) 1)
    

(def test-cases [{:x -10 :y -1007}
                 {:x -8 :y -517}
                 {:x -6 :y -219}
                 {:x -4 :y -65}
                 {:x -2 :y -7}
                 {:x 0 :y 3} 
                 {:x 1 :y 5}
                 {:x 3 :y 33}
                 {:x 5 :y 133}
                 {:x 7 :y 353}
                 {:x 9 :y 741}])

(defn fitness-prog
  "This function finds the fitness of a single program."
  [program test-cases]
  (apply + 
         (map (fn[test-case] 
                (let [x (:x test-case)
                      y (:y test-case)]
                  (Math/abs (- (eval program) y))))
              test-cases)))

(fitness-prog '(+ (* (* x x) x) (+ x 3)) test-cases)

(defn fitness-eval
  "This function creates a map of functions with their
   respective error values."
  [population test-cases]
  (map (fn [program]
         {:function program :fitness (fitness-prog program test-cases)}) 
       population))
    
(defn single-tourney
  "This function runs a single round of a tournament."
  [& programs]
  (map keys programs))

(single-tourney {2 3} {4 3})


(defn tournmanent-selection
  "This function generates a list of programs which can
  be used as parents for the next generation."
  [pop-map]
  (take (rand-int 7) (repeatedly #(rand-nth pop-map))))

(defn erc
  "This function is an ephemeral random constant."
  []
  (rand-nth (range -50 50)))

(defn pd
  "Protected division, to protect against divide by zero."
  [x y]
  (if (= y 0)
    1
    (/ x y)))

(def instructions
  '{+ 2
    * 2
    - 2
    pd 2
    inc 1})

(defn program-size
  [prog]
  (if (seq? prog)
    (count (flatten prog))
    1))
  
(defn select-random-subtree
  "Given a program, selects a random subtree and returns it."
  ([prog]
    (select-random-subtree prog (rand-int (program-size prog))))
  ([prog subtree-index]
    (cond
      (not (seq? prog)) prog
      (and (zero? subtree-index)
           (some #{(first prog)} (keys instructions))) prog
      (< subtree-index (program-size (first prog))) (recur (first prog)
                                                           subtree-index)
      :else (recur (rest prog)
                   (- subtree-index (program-size (first prog)))))))

(select-random-subtree prog)
(select-random-subtree prog 0) ;gives subtree at index 0
(select-random-subtree prog 1) ;gives subtree at index 1
(select-random-subtree prog 2) ;gives subtree at index 2

(defn replace-random-subtree
  "Given a program and a replacement-subtree, replace a random node
   in the program with the replacement-subtree."
  ([prog replacement-subtree]
      (replace-random-subtree prog replacement-subtree (rand-int (program-size prog))))
  ([prog replacement-subtree subtree-index]
    (cond
      (not (seq? prog)) replacement-subtree
      (zero? subtree-index) replacement-subtree
      :else (map (fn [element start-index]
                   (if (<= start-index
                           subtree-index
                           (+ start-index -1 (program-size element)))
                     (replace-random-subtree element
                                             replacement-subtree
                                             (- subtree-index start-index))
                     element))
                 prog
                 (cons 0 (reductions + (map program-size prog)))))))

prog

(replace-random-subtree prog 99)

(defn crossover
  "This function performs a crossover on two programs and
  returns the new program."
  [prog1 prog2]
  (replace-random-subtree prog2 (select-random-subtree prog1)))

(crossover '(+ (* 2 3) 2) '(- 3 2))

(defn mutation
  "This function takes a random node in a subtree and replaces it
  with another random node."
  [program terminal-set function-set max-depth]
  (replace-random-subtree program (if (> (rand-int 10) 8)
                                    (rand-terminal terminal-set)
                                    (grow-tree terminal-set function-set max-depth))))

(mutation '(+ (* 2 3) 2) '((erc) x) '(+ - pd *) 4)

(defn genetic-programming
  "This function runs the genetic programming system.
  For every generation, it displays the generation number,
  the best program of that generation, and the total error
  of the best program."
  []
  (let [function-set '(+ - / *)
        terminal-set '((erc) x)
        max-depth 5
        pop-size 1000
        max-gen 10]
    (let [initial-pop (make-population pop-size terminal-set function-set max-depth)]
      )))




    

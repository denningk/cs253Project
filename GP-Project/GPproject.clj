(ns cs253.GPproject)

;; Keith Denning and Rajwol Joshi

;; Initialization

(defn rand-frontier-tree
  "This function generates a random subtree with one function
  and its arguments."
  [terminal-set function-set]
  (list (rand-nth function-set) (rand-nth terminal-set) (rand-nth terminal-set)))

(defn rand-terminal
  "This function returns a random terminal from the terminal set."
  [terminal-set]
  (list (rand-nth terminal-set)))
                                                                                                                                
; Tree Generation
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

(defn grow-tree
  "This function creates a random grow-tree"
  [terminal-set function-set max-depth]
  (let [item (rand-nth (concat function-set terminal-set))]
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
  
; Population Generation
(defn gen-population
  "This program generates a population of
   randomly generated programs. Uses a loop."
  [num-pop max-depth terminal-set function-set]
  (loop [pop '()
         num num-pop]
    (if (= num 0)
      pop
      (recur (conj pop (if (< (rand-int 10) 5)
                         (full-tree terminal-set function-set max-depth)
                         (grow-tree terminal-set function-set max-depth)))
             (dec num)))))

(defn make-population
  "Makes a population of n number of program-trees of a inputed max-depth 
using terminal-set and function-set. Does not use a loop."
  [pop-num terminal-set function-set max-depth]
  (list (take (* 0.5 pop-num)(repeatedly #(full-tree terminal-set function-set max-depth)))
        (take (* 0.5 pop-num)(repeatedly #(grow-tree terminal-set function-set max-depth)))))
    
;; Fitness Evaluation
; Use 10 data points
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

(defn make-program-into-fn
  [program]
  (eval (list 'fn
              '[x]
              program)))

(defn fitness-prog
  "This function finds the fitness of a single program."
  [program test-cases]
  (apply +' 
         (map (fn[test-case] 
                (let [x (:x test-case)
                      y (:y test-case)
                      func-program (make-program-into-fn program)] ; In case 'program' is a terminal
                  (Math/abs (int (- (if (= 1 (number? program))    ; Otherwise causes cast error
                                      (nth program 0 1)
                                      (func-program x)) 
                                    y)))))
              test-cases)))

(defn fitness-eval
  "This function creates a map of functions with their
   respective error values."
  [population test-cases]
  (map (fn [program]
         {:function program :fitness (fitness-prog program test-cases)})
       population))

;; Parent Selection
; We decided to implement tournament selection

(defn single-tourney
  "This function runs a single round of a tournament.
  It returns the function which wins the tourney. If there
  is a tie, the winner is chosen randomly."
  [&programs]
  (let [best-error (apply min (map :fitness &programs))]
   (:function (rand-nth (filter #(= best-error (:fitness %)) &programs)))))

(defn tournament-selection
  "This function generates a list of programs which can
  be used as parents for the next generation. 2 to 4 programs
  are chosen to participate in each round."
  [pop-maps]
  (repeatedly (count pop-maps) #(single-tourney 
                                 (repeatedly (rand-nth (range 2 4)) (fn [](rand-nth pop-maps))))))

; Ephemeral Random Constant defined
(defn erc
  "This function is an ephemeral random constant."
  []
  (rand-nth (range -50 50)))

; Protected division
(defn pd
  "Protected division, to protect against divide by zero."
  [x y]
  (if (= y 0)
    1
    (/ x y)))

;; Variation

(def instructions
  '{+ 2
    * 2
    - 2
    pd 2})

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

(defn crossover
  "This function performs a crossover on two programs and
  returns the new program."
  [prog1 prog2]
  (replace-random-subtree prog2 (select-random-subtree prog1)))

(defn mutation
  "This function takes a random node in a subtree and replaces it
  with another random node."
  [program terminal-set function-set max-depth]
  (replace-random-subtree program (if (> (rand-int 10) 5) ; Replaces with a terminal 50% of time
                                    (rand-terminal terminal-set)
                                    (grow-tree terminal-set function-set max-depth))))

; Combines all variation techniques
(defn evolve
  "This function takes a collection of selected parents and evolves them."
  [parents terminal-set function-set max-depth]
  (loop [new-pop '()
         evolve-num (count parents)]
    (let [probability (rand-int 100)] ; Reproduction 10%, Mutation 40%, Crossover 50%
      (cond (and (> probability 90) (> evolve-num 0)) (recur (conj new-pop (rand-nth parents))
                                                             (dec evolve-num))
            (and (> probability 50) (> evolve-num 0)) (recur (conj new-pop (mutation (rand-nth parents) terminal-set function-set max-depth))
                                                             (dec evolve-num))
            (and (> probability 0) (> evolve-num 0)) (recur (conj new-pop (crossover (rand-nth parents) (rand-nth parents)))
                                                            (dec evolve-num))
            :else new-pop))))

(defn best-fitness
  "This functions finds thes the function with the best fitness
  and outputs its value"
  [pop-maps]
  (apply min (map :fitness pop-maps)))

;; Main Function

(defn genetic-programming
  "This function runs the genetic programming system.
  For every generation, it displays the generation number,
  the best program of that generation, and the total error
  of the best program."
  []
  (let [function-set '(+ - * pd)
        terminal-set '(1 2 3 x)
        max-depth 2
        pop-size 5
        max-gen 3]
    (loop [current-pop (gen-population max-depth pop-size terminal-set function-set) ; Initialize population
           gen-num 0
           best-program nil
           best-prog-error nil]
      (println "Generation Number:" gen-num
               "\nThe best program was:" best-program
               "\nThe error of the best program was:" best-prog-error)
      (if (or (= gen-num max-gen) (= best-prog-error 0)) ; Continue until solution found or if max generation reached
        (do (println "\nGenetic Evolution finished!")
          (println "The best program is:")
          best-program)
        (let [current-fitness (fitness-eval current-pop test-cases) ; Fitness evaluation
              best-prog (single-tourney current-fitness) ; Finds best program of generation
              best-prog-fitness (best-fitness current-fitness)]
          (recur (evolve (tournament-selection current-fitness) ; Selection and evolution
                         terminal-set function-set max-depth)
                 (inc gen-num)
                 best-prog
                 best-prog-fitness))))))

;; An error continues to occur in the "fitness-eval" function. If a program is a single terminal,
;; a cast error is thrown because those single-terminal programs cannot be used as funcions. We attempted
;; to fix this error but we were not able to even though the code that we added seems like it should work.
;; If the genetic programming system is run enough times, it will eventually run all the way through with a low
;; population and low generation.

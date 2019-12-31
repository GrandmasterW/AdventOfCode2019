(ns aoc2019.day2)

;;
;; Day 2 - Part 1
;; simple intcode computer
;;

(def program [
1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,9,19,23,1,6,23,27,1,10,27,31,1,5,31,35,2,6,35,39,1,5,39,43,1,5,43,47,2,47,6,51,1,51,5,55,1,13,55,59,2,9,59,63,1,5,63,67,2,67,9,71,1,5,71,75,2,10,75,79,1,6,79,83,1,13,83,87,1,10,87,91,1,91,5,95,2,95,10,99,2,9,99,103,1,103,6,107,1,107,10,111,2,111,10,115,1,115,6,119,2,119,9,123,1,123,6,127,2,127,10,131,1,131,6,135,2,6,135,139,1,139,5,143,1,9,143,147,1,13,147,151,1,2,151,155,1,10,155,0,99,2,14,0,0])
;;

(defn int-step [program position]
  "processes the step at the current position of program. 1: add, 2: mult, 99: exit else exit. Returns vector of new program memory status and next position"
  (let [opcode (get program position)
        addr1 (get program (+ position 1))
        addr2 (get program (+ position 2))
        target (get program (+ position 3))
        op1 (get program addr1)
        op2 (get program addr2)
        next-pos (+ position 4)]
    (case opcode
      1 [(assoc program target (+ op1 op2)) ; updated
         next-pos] ; new position to continue
      2 [(assoc program target (* op1 op2)) ; updated memory
         next-pos] ; new position to continue
      99 [program nil]
      [program nil]))) ; in case of not found or wrong code
  
(defn intrun [program position]
  "takes a vector for the current program memory and the next position to be processed. Returns last memory status, i.e. resulting program memory"
  (let [[newprog newpos] (int-step program position)] ; process current step
    (if-not newpos
      newprog ; return last memory layout for program, as end is reached
      (recur newprog newpos)))) ; if position valid then continue from next position

(defn intstart [program]
  (intrun program 0))

;; result: 6087827 (OK) 

;; Part 2
;; changing noun (address 1) and verb (address 2) to result in 19690720
;;
(def result 19690720)
(def noun-pos 1) ; probably bad style - constants
(def verb-pos 2)

(defn set-noun-verb  [program noun verb]
  "returns the changed program for noun and verb"
  (let [n-prog (assoc program noun-pos noun)]
    (assoc n-prog verb-pos verb)))

(defn set-vars-and-start [program noun verb]
  "sets the noun and verb in the program memory and delivers the result together with noun and verb"
  (let [res-prog (intstart (set-noun-verb program noun verb))
        result-value (get res-prog 0)]
    [result-value noun verb]))


(defn find-day2-vars [program result]
  "find all variations (should be only one) which deliver the resulting value in address 0. Return the noun and verb for those."
  (let [to-range (min (count program) 99)]
    (filter #(= result (first %1))
            (for [noun (range to-range)
                  verb (range to-range)]
              (set-vars-and-start program noun verb)))))
;; [19690720 53 79] --> 5379 (OK) 

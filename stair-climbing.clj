;; Copyright (C) 2009 Michel Alexandre Salim.
;; see LICENSE.TXT for licensing

;; stair-climbing robot, by Chung-chieh Shan:

"          Programming (language) puzzles
           Shan, Chung-chieh (2006).
           http://lambda-the-ultimate.org/node/1872

Your stair-climbing robot has a very simple low-level API: the 'step'
function takes no argument and attempts to climb one step as a side
effect. Unfortunately, sometimes the attempt fails and the robot
clumsily falls one step instead. The 'step' function detects what
happens and returns a boolean flag: true on success, false on
failure. Write a function 'step_up' that climbs one step up (by
repeating 'step' attempts if necessary). Assume that the robot is not
already at the top of the stairs, and neither does it ever reach the
bottom of the stairs. How small can you make 'step_up'? Can you avoid
using variables (even immutable ones) and numbers?
"
;; the initial level
(def level (atom 41))

;; the probability of success
(def prob 0.5001)

(defn step
  "The stepper function. Attempts to go up one level, but might fail and
   go down one instead. Returns true if succeeds, false otherwise.

   Assumption: no top floor, no bottom floor."
  []
  (let [success (< (rand) prob)]
    (swap! level (if success inc dec))
    success) )

(defn step-up1
  "Straightforward implementation: keep track of how many level we
   need to ascend, and stop when this count is zero."
  []
  (loop [deficit 1]
    (or (zero? deficit)
	(recur (if (step) (dec deficit)
		   (inc deficit)))) ) )

(defn step-up2
  "Non-tail-recursive. No numbers."
  []
  (if (not (step))
    (do (step-up2) ;; undo the fall
	(step-up2) ;; try again
	)
    true))

;; Copyright (C) 2009 Michel Alexandre Salim.
;; see LICENSE.TXT for licensing


;; Port of Kernighan and Pike's simple regex matcher

"          The Practice of Programming
           Kernighan, Brian and Pike, Rob.

           Beautiful Code
           Kernighan, Brian
           (ed. Oram, Andy and Wilson, Greg)
"

(defn match
  "match searches for re anywhere in the text"
  [re text]
  (let [rlen (.length re)
	tlen (.length text)]
    (letfn [(match-here ;;match-here searches for re[ri:] starting at text[ti:]
	     [ri ti]
	     (cond (= ri rlen) true
		   (= (get re (inc ri)) \*)
		     (match-* (get re ri) (+ ri 2) ti)
		   (and (= (get re ri) \$)
			(= (inc ri) rlen))
		     (= ti tlen)
		   (and (not (= ti tlen))
			(or (= (get re ri) \.)
			    (= (get re ri) (get text ti))))
		     (recur (inc ri) (inc ti))
		   :else false))
	    (match-* ;; match-* searches for c*re[ri:] starting at text[ti:]
	     [c ri ti]
	     (loop [ti ti]
	       (cond (match-here ri ti) true
		     (and (< ti tlen)
			  (or (= (get text ti) c)
			      (= c \.)))
		     (recur (inc ti))
		     :else false))) ]
      (if (= (get re 0) \^)
	(match-here 1 0)
	(loop [ti 0]
	  (cond (match-here 0 ti) true
		(= ti tlen) false
		:else (recur (inc ti))))))))


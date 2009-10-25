;; Copyright (C) 2009 Michel Alexandre Salim.
;; see LICENSE.TXT for licensing

;; Solving the "Mr.S and Mr.P" puzzle by John McCarthy:

"          Formalization of two Puzzles Involving Knowledge
           McCarthy, John (1987).
           http://www-formal.stanford.edu/jmc/puzzles.html

We pick two numbers a and b, so that a>=b and both numbers are within
the range [2,99]. We give Mr.P the product a*b and give Mr.S the sum
a+b.

The following dialog takes place:

	Mr.P: I don't know the numbers
	Mr.S: I knew you didn't know. I don't know either.
	Mr.P: Now I know the numbers
	Mr.S: Now I know them too

Can we find the numbers a and b?

The code is a direct translation of Oleg Kiselyov's Haskell solution:

  http://okmij.org/ftp/Haskell/Mr-S-P.lhs
"

;; The good numbers
(def good-nums (range 2 100))

;; Given a number p, find all good factors a and b s.t. a >= b
;; Using memoization, this "table" is a lazy sequence containing
;; all possible products
(def good-factors-table
     (letfn [(gf [p]
		 (for [a good-nums
		       b good-nums
		       :when (and (>= a b)
				  (= (* a b) p))]
		   [a b]))]
       (pmap gf (iterate inc 0))))

;; To find all good factors for p, just index into the table
(defn good-factors [p]
  (nth good-factors-table p))

;; Given a number s, find all good summands a and b s.t. a >= b
;; Same technique as before
(def good-summands-table
     (letfn [(gs [s]
		 (for [a good-nums
		       b good-nums
		       :when (and (>= a b)
				  (= (+ a b) s))]
		   [a b]))]
       (pmap gs (iterate inc 0))))

;; To find all good summands for s, index into the table
(defn good-summands [s]
  (nth good-summands-table s))

(defn singleton?
  "true iff xs contains a single element"
  [xs]
  (and (not (empty? xs))
       (empty? (next xs))))

(defn fact1?
  "Mr.P does not know the numbers, therefore the product does *not*
   have a unique factorization"
  [[a b]]
  (not (singleton? (good-factors (* a b)))))

(defn fact2?
  "Mr.S does not know the numbers, ditto with the sum"
  [[a b]]
  (not (singleton? (good-summands (+ a b)))))

(defn fact3?
  "Mr.S knows Mr.P does not know. All the good summands must not
   have unique factorizations"
  [[a b]]
  (every? fact1? (good-summands (+ a b))))

(defn fact4?
  "Mr.P *now* knows fact3 is true, and can find the numbers. Thus
   only one factorization makes fact3 true"
  [[a b]]
  (singleton? (filter fact3? (good-factors (* a b)))))

(defn fact5?
  "Mr.S knows Mr.P found the numbers, therefore only one decomposition
   of a+b makes fact4 true"
  [[a b]]
  (singleton? (filter fact4? (good-summands (+ a b)))))


;; the list of all numbers such that fact1..fact5 holds
(def result (for [a good-nums
		  b good-nums
		  :when (and (>= a b)
			     (every? #(% [a b])
				     [fact1? fact2? fact3? fact4? fact5?]))]
	      [a b]))

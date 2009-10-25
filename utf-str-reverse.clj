;; Copyright (C) 2009 Michel Alexandre Salim.
;; see LICENSE.TXT for licensing

;; Unicode-safe string reverse

"This is an initial implementation, and passes the Rosetta Code test case:
 http://rosettacode.org/wiki/Reversing_a_string

 Need to investigate:
 - whether there are other character types that are combining
 - the Java documentation does not make much sense:
   code 6 and 7 are not marked as Unicode...
   http://java.sun.com/javase/6/docs/api/constant-values.html#java.lang
"

(defn combining? [c]
  (let [type (Character/getType c)]
    (or (= type 6) (= type 7))))

(defn group
  "Group the characters such that each group contains exactly one non-combining character,
   at the front. Preserves ordering"
  [chars]
  (cond (empty? chars) chars
	(empty? (next chars)) (list chars)
	:else
	(let [dres (group (next chars))]
	  (cond (combining? (second chars)) (cons (cons (first chars)
							(first dres))
						  (rest dres))
		:else (cons (list (first chars)) dres)))))

(defn str-reverse
  "Unicode-safe string reverse"
  [s]
  (apply str (apply concat (reverse (group s)))))

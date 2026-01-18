(***
----------------------------------------------------------- 
globals.mli
----------------------------------------------------------- 
global constants for precision and tolerance thresholds.
context matters on which value to use:
- precision: for geometric comparisons where small differences matter
- epsilon: for avoiding division by zero and degenerate cases
***)

val precision : float
val epsilon : float

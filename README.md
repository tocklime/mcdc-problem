# mcdc

This is some code to play with a problem I came across: generating and checking MCDC coverage for a given boolean expression.

Currently it works for expressions with no repeating items, such as `(A && B) || C`, but not for items which have repeated items and also have a valid MCDC solution: `(!A && B) || (A && C)`. 

## Algorithm

* MCDC coverage for a single literal `A` is `(!A,A)`. The first element of the pair is the list of truth vectors which end up false, the second is the list of truth vectors which end up true.
* MCDC coverage for an expression `!A` is the same as the solution for `A`, but with the 2 values in the tuple swapped (false becomes true, true becomes false)
* MCDC coverage for an expression `A && B` where A is covered by `(A_Falses,A_Trues)` and B is covered by `(B_Falses,B_Trues)` is done as follows:
  * Calculate set A: vectors in `A_Trues` and pair them with a vector from `B_trues`<sup>[1](#vector-selection)</sup>.
  * Calculate set B: vectors in `A_falses` and pair them with a vector from `B_trues`<sup>[1](#vector-selection)</sup>.
  * Calculate set C: vectors in `B_falses` and pair them with a vector from `A_trues`<sup>[1](#vector-selection)</sup>.
  * Calculate set D: vectors in `B_Trues` and pair them with a vector from `A_trues`<sup>[1](#vector-selection)</sup>.
  * The result is `(B+C,A+D)`
* MCDC coverage for or can be done by using the equivalence `(A || B) == !(!A && !B)`

<b id='vector-selection'>1:</b> The selection of a vector is a crucial decision. You must not try to merge two vectors with a conflicting value for a given variable. Ideally, we would use a backtracking algorithm in this selection, and so get all possible MCDC coverage sets.

## Haddock docs

[You can see the documentation and source code here](https://tocklime.github.io/mcdc-problem/docs/index.html)

## Whats next

* ~~Implement a naive powerset-based MCDC coverage solution (for checking more carefully constructed solutions)~~
* Make the selection of vectors to pair with slightly better - at the moment it selects the 'first'. It would do better to select the first that does not conflict.
* find equations which the naive one can solve, but the constructor can't (I expect to see things like `(!A && B) || (A && C)` here) & fix those. 
* Find out if it's possible to alter the algorithm to handle `A && A`. If not, then implement some boolexp simplifier (Quine and McCluskey?)
* Extend the constructive generator to generate all possible sets, rather than maybe one.
* Implement a Masking / short circuiting version.


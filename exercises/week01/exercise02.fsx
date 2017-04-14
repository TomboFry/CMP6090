// CMP6090: Week 1 Exercise 2
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   The logical implication operator has the following truth table:

   p   q   p->q
   T   T   T
   T   F   F
   F   T   T
   F   F   T

   Write an F# function called 'implies' that will have the following type
   signature:

   `val implies : p: bool -> q: bool -> bool`

   You should use match expressions to pattern match on input values.
   You may also use `if' expressions if you wish
*)

// Solution:

let implies p q =
 match (p, q) with
 | (true, true)   -> true  // Explicitly stating all results even though
 | (true, false)  -> false // there is a simpler method.
 | (false, true)  -> true
 | (false, false) -> true

// Test:

let outputA = implies true true // Expected true
let outputB = implies true false // Expected false

printfn "%b, %b" outputA outputB
// CMP6090: Week 1 Exercise 3
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   The following is the truth table for a half-adder circuit:

   Input   Output
   A   B   C   S
   0   0   0   0
   0   1   0   1
   1   0   0   1
   1   1   1   0

   Implement this function in F#

   (Note that this function returns two values, but functions should only return
    one value! Is there any way that we could return the pair of values
    together?)
*)

// Solution:

let halfadder a b =
 match (a, b) with
 | (false, false) -> (false, false) // Again, stating each match explicitly.
 | (false, true)  -> (false, true)  // Though this time, we are returning a
 | (true, false)  -> (false, true)  //   tuple in order to get that 
 | (true, true)   -> (true, false)

// Test:

let (outputAA, outputAB) = halfadder true false
let (outputBA, outputBB) = halfadder true true

printfn "(%b, %b)" outputAA outputAB
printfn "(%b, %b)" outputBA outputBB
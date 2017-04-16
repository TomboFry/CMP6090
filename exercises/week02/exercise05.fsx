// CMP6090: Week 2 Exercise 5
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a series of functions called

   - fstTrpl
   - sndTrpl
   - thrdTrpl

   Each of which takes in a 3-tuple and then returns the first, second, or third
   elements respectively
*)

// Solution:

let fstTrpl (x, _, _) =
 x

let sndTrpl (_, x, _) =
 x

let thrdTrpl (_, _, x) =
 x

// Test:

let tuple = (0, 5, 1)

let resA = fstTrpl tuple
let resB = sndTrpl tuple
let resC = thrdTrpl tuple

// Expected output: `(0, 5, 1)`
printfn "(%i, %i, %i)" resA resB resC

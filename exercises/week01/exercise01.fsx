// CMP6090: Week 1 Exercise 1
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a function that 'cubes' its input and returns a value of type float.
   You are not allowed to use the ** operator or the * operator, but you are
   allowed to use the pown function
*)

// Solution:

let cube num = pown num 3

// Test:
let output = cube 5

// Expected: `125`
printfn "%i" output

// CMP6090: Week 2 Exercise 1
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Create a list of the first 10 odd numbers, starting from 1. You must use the
   'cons syntax' to define this list.
*)

// Solution:

let list = 1 :: 3 :: 5 :: 7 :: 9 :: 11 :: 13 :: 15 :: 17 :: 19 :: []

// Test:

// Expected: `[1; 3; 5; 7; 9; 11; 13; 15; 17; 19]`
printfn "%A" list

// CMP6090: Week 1 Exercise 5
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a simple version of the `echo` Linux command line utility in F#.

   Think carefully about how this program works from the perspective of the user
*)

// Solution:

[<EntryPoint>]
let main args =
 for arg in args do
  printf "%s " arg
 0
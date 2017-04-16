// CMP6090: Week 2 Exercise 7
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a "How many days in a month? (with leap year case covered)" to make use
   of a discriminated union when matching on months. This should allow you to
   protect against incorrect inputs to the function by leveraging the type
   system.
*)

// Solution:

type Month =
 | January

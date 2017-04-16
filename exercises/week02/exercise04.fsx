// CMP6090: Week 2 Exercise 4
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Implement the logical `and` function (&&) on the previously defined
   BoolCustom type
*)

// Solution:

// Not technically boolean, so && and ! will not work with this type, hence the
// implementation.
type BoolCustom =
 | TRUE
 | FALSE

let andCustom x y =
 if x = y then
  TRUE
 else
  FALSE

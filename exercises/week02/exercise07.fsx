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
 | February
 | March
 | April
 | May
 | June
 | July
 | August
 | September
 | October
 | November
 | December

let daysInMonth month year =
 match month with
 | February when (year % 4 = 0) -> 29
 | February -> 28
 | April | June | September | November -> 30
 | _ -> 31

// Test:

let resultA = daysInMonth January 2005
let resultB = daysInMonth February 2006
let resultC = daysInMonth February 2008
let resultD = daysInMonth April 1996

// Expected: `31, 28, 29, 30`
printfn "%i, %i, %i, %i" resultA resultB resultC resultD

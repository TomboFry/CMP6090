// CMP6090: Week 2 Exercise 6
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Make a function called GCD (Greatest Common Denominator) which works out the
   largest integer which evenly divides the int parameters `x` and `y`.

   Remember that we can use the % (modulus) operator to calculate the remainder
   when dividing one number by another.

   Example expression reduction:
   ```
   gcd 518 444
   gcd 444 (518 % 444)
   gcd 444 (74)
   gcd 74 (444 % 74)
   gcd 74 (0)
   74
   ```
*)

// Solution:

let rec gcd x y =
 if y = 0u then
  x
 elif y > 0u then
  gcd y (x % y)
 else
  gcd y x

let doparse (args:string[]) idx =
 if args.Length > idx then
  try
   uint32 args.[idx]
  with
  | :? System.FormatException -> printfn "Invalid number, defaulting to 0\n"
                                 0u
  | :? System.OverflowException -> printfn "Invalid number, defaulting to 0\n"
                                   0u
 else
  printfn "Incorrect number of arguments, defaulting to 0\n"
  0u

[<EntryPoint>]
let main args =
 let numA = doparse args 0
 let numB = doparse args 1

 let result = gcd numA numB
 printfn "%u" result
 0

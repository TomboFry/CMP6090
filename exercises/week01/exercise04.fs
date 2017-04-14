// CMP6090: Week 1 Exercise 4
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write FizzBuzz.

   Take in an integer `n` and do the following:
   - Print "Fizz" if `n` is divisible by 3
   - Print "Buzz" if `n` is divisible by 5
   - Print "FizzBuzz" if `n` is divisible by both 3 and 5
   - Print `n` otherwise
*)

// Solution:

let fizzbuzz n =
 if n % 3 = 0 && n % 5 = 0 then
  "FizzBuzz"
 elif n % 3 = 0 then
  "Fizz"
 elif n % 5 = 0 then
  "Buzz"
 else
  string n

// Test:

let rec readline() =
 match System.Int32.TryParse(System.Console.ReadLine()) with
 | (true, index) -> index
 | _ -> readline()


[<EntryPoint>]
let main args =
 printf "Enter a number:\n> "

 let index =
  if args.Length > 0 then
   try
    int args.[0]
   with
   | :? System.FormatException -> printfn "Invalid number, defaulting to 0"
                                  0
  else
   readline()

 let result = fizzbuzz index
 printfn "\nResult: %s" result
 0
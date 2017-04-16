// CMP6090: Week 2 Exercise 3
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a function that **recursively** prompts for an int input using
   `Console.ReadLine()`. If the given string provided as input is a valid 32-bit
   signed integer; if the input string can be converted to a valid 32-bit int it
   returns that int value, if not it tells the user that there was an input
   error and then prompts for the input again
*)

// Solution:

let rec readline() =
 match System.Int32.TryParse(System.Console.ReadLine()) with
 | (true, index) -> index
 | _ -> printf "Not a valid integer! Try again:\n> "
        readline()

[<EntryPoint>]
let main _ =
 printf "Enter a number:\n> "
 let result = readline()
 printfn "\n%i" result
 0

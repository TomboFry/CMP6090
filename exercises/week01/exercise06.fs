// CMP6090: Week 1 Exercise 6
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a program that prompts the user for two floating point numbers `x` and
   `y` as input and then multiplies them together.
*)

// Solution:

let rec readline() =
 match System.Double.TryParse(System.Console.ReadLine()) with
 | (true, index) -> index
 | _ -> printf "Not a valid number! Try again:\n> "
        readline()

let userint (args:string[]) idx =
 if args.Length > idx then
  try
   float args.[idx]
  with
  | :? System.FormatException -> printfn "Invalid number, defaulting to 0"
                                 0.0
 else
  printf "\nEnter a number:\n> "
  readline()

[<EntryPoint>]
let main args =
 printfn "Multiply two numbers together"
 let numA = userint args 0
 let numB = userint args 1

 let result = numA * numB
 printfn "\n%f" result
 0

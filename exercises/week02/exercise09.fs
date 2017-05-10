// CMP6090: Week 2 Exercise 9
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a program that mimics the Linux command line utility `tac` with no
   options specified.

   Example:
   ```
   $ cat file.txt
   the quick brown fox
   jumps over the lazy dog
   $ tac file.txt
   jumps over the lazy dog
   the quick brown fox
   ```
*)

// Solution:

let revFile filePath =
 try
  let lines = System.IO.File.ReadLines(filePath)

  lines
  |> Seq.rev
  |> Seq.iter (fun x -> printfn "%s" x)

 with
 | :? System.IO.FileNotFoundException -> printfn "tac: failed to open '%s' for reading: No such file or directory" filePath

[<EntryPoint>]
let main args =
 if args.Length = 0 then
  printfn "usage: tac [files...]"
 else
  for file in args do
   revFile file
 0

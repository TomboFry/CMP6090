// CMP6090: Week 2 Exercise 8
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a program that mimics the Linux command line utility `rev` with no
   options specified.

   Example:
   ```
   $ cat file.txt
   the quick brown fox
   jumps over the lazy dog
   $ rev file.txt
   xof nworb kciuq eht
   god yzal eht revo spmuj
   ```
*)

// Solution:

let revLine (s:string) = System.String(s.ToCharArray() |> Array.rev)

let revFile filePath =
 try
  let lines = System.IO.File.ReadLines(filePath)
  lines |> Seq.iter (fun x -> printfn "%s" (revLine x))
 with
 | :? System.IO.FileNotFoundException -> printfn "rev: Cannot open %s: No such file or directory" filePath

[<EntryPoint>]
let main args =
 if args.Length = 0 then
  printfn "usage: rev [files...]"
 else
  for file in args do
   revFile file
 0

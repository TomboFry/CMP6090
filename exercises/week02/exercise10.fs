// CMP6090: Week 2 Exercise 10
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Write a program that mimics the Linux command line utility `wc` with no
   options specified. Remember, according to the `wc` man file: wc prints
   newline, word, and byte counts for each FILE, and a total line if more than
   one FILE is specified. A word is a non-zero length sequence of characters
   delimited by white space.

   Example:
   ```
   $ wc file.txt
    1  9 43 file.txt
   $ wc file.txt file2.txt
    1  9 43 file.txt
    0  5 28 file2.txt
    1 14 71 total
   ```
*)

// Solution:

// let byteCount filePath =


let loadLines filePath =
 try
  System.IO.File.ReadLines(filePath)
 with
 | :? System.IO.FileNotFoundException -> printfn "wc: %s: No such file or directory" filePath
                                         failwith ""

let loadBytes filePath =
 try
  System.IO.File.ReadAllBytes(filePath)
 with
 | :? System.IO.FileNotFoundException -> printfn "wc: %s: No such file or directory" filePath
                                         failwith ""


let processFile filePath =
 let lines = loadLines filePath |> Seq.length
 printf " %5i" lines

 let words =
  [|
   let lines = loadLines filePath
   for line in lines do
    let words = line.Split([| ' '; '\n'; '\r'; '\t'; |])
    yield words.Length
  |]
  |> Array.sum

 printf " %5i" words

 let bytes = loadBytes filePath |> Seq.length
 printf " %5i" bytes
 printfn " %s" filePath

 (lines, words, bytes)

[<EntryPoint>]
let main args =
 if args.Length = 0 then
  printfn "usage: wc [FILE]..."
  1
 else
  let files = Array.map processFile args
  if args.Length > 1 then
   let (lines, words, bytes) =
    Array.fold (fun (lines_a, words_a, bytes_a) (lines_b, words_b, bytes_b) ->
    (lines_a + lines_b, words_a + words_b, bytes_a + bytes_b)) (0,0,0) files
   printfn " %5i %5i %5i total" lines words bytes
  0

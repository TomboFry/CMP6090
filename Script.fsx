// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
open CMP6090

let num = Library.hello 42
let num2 = Library.test 3

printfn "%i" num
printfn "%i" num2
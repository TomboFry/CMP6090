// CMP6090: Week 2 Exercise 2
// Thomas Gardiner <Thomas.Gardiner3@mail.bcu.ac.uk>

(*
   Create a dictionary that allows you too look up the email address and gender
   of you and three more people.
*)

// Solution:

// There are only two genders. There are only two genders.
type Gender =
 | Male
 | Female

type Person = {
 gender: Gender;
 email: string
}

let people = dict [
              "Tom Gardiner",  { gender=Male;   email="Thomas.Gardiner3@mail.bcu.ac.uk" };
              "Someone Else",  { gender=Female; email="some@email.com" };
              "Friendly Name", { gender=Female; email="friendlyname@gmail.com" };
              "Final Person",  { gender=Male;   email="lastperson@name.me" }
             ]

// Test:

// Expected: `Male, "Thomas.Gardiner3@mail.bcu.ac.uk"`
printfn "%A, %A" people.["Tom Gardiner"].gender people.["Tom Gardiner"].email

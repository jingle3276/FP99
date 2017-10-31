

open System



let myFunc (x:int) = 
    sprintf "%i" x


let myFunc1: (int seq -> string seq) = Seq.map myFunc 
let myFunc2: (int option -> string option) = Option.map myFunc


//let myFunc2a = Option.


[1;1;1;2;2;2;3;3;3]
|> Seq.distinctBy id
|> printfn "%A"

let predicate s = 
    s <> "JD" && s <> "NJ" 




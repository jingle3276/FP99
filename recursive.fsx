open System

let arr =
    [|
        "abcd"
        "df"
        "double"
    |]

arr |> Array.toList

let f = function num -> num = 10
f 11


// let getFunc (arr: string array) =
//     let rec aux (acc: int->bool) (ls: string list): (int->bool) =
//         match ls with
//         | [] -> acc
//         | head::tail ->
//             //function num -> 
//             let headFunc num =
//                 match ((String.length head) % num) with
//                 | 0 -> true
//                 | _ -> false
//             let aa num = (headFunc num) && (acc num)
//             aux aa tail
//     let initFunc (_:int) = true
//     aux initFunc (arr |> Array.toList)

// let func1 = getFunc arr 

/// this function returns a function that takes a char and determine 
/// whether this char is in one of the input array of strings
let getFuncContainsAnyChar (ls: string list): (char -> bool) = 
    let rec aux (acc: char -> bool) (ls: string list): char -> bool =
        match ls with 
        | [] -> acc
        | head::tail ->     
            let headFunc inputChar =
                head |> Seq.contains inputChar
            let orFuncs inputChar = (headFunc inputChar) || (acc inputChar)
            aux orFuncs tail
    aux (function _ -> false) ls    // must be false initially 


let myMap =
    [("1", "a"); ("2", "b")]
    |> Map.ofList

let r = Map.tryFind "2" myMap
let o = defaultArg r "c"







let d = decimal 0.0

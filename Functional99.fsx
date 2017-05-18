// https://ocaml.org/learn/tutorials/99problems.html

// Working with lists

// Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
let rec last (list: 'a list): 'a option =
    match list with
    | [] -> None
    | [a] -> Some a
    | _ :: tail_list -> last tail_list
// Note:  None in F# has to have a type !

let addOne (x: int) = 
   x + 1
let newFunc = last >> Option.map addOne  


// Find the last two items in a list (easy)
let rec lastTwo (list: 'a list) =
    match list with
    | [] -> None
    | [a; b] -> Some (a, b)
    | _ :: tail_list -> lastTwo tail_list

// Find the k'th element of a list. (easy)
let rec findKth (list: 'a list) (k: int): 'a option = 
    match (k, list) with
    | 0, head::_ -> Some head 
    | _, _::tail -> findKth tail (k-1)
    | _ -> None 


let rec reverse' acc aList =
    match aList with
    | [] -> acc
    | head :: tail -> reverse' (head::acc) tail 


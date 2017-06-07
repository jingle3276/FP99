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


// Find the number of elements of a list. (easy)
// Bonus for a tail recursive solution.
let length aList = 
    let rec aux acc aList = 
        match aList with 
        | [] -> acc
        | _::tail -> aux (acc + 1 ) tail
    aux 0 aList



// Flatten a nested list structure. (medium)
// (* There is no nested list type in OCaml(FSharp), so we need to define one
//      first. A node of a nested list is either an element, or a list of
//      nodes. *)
//   type 'a node =
//     | One of 'a 
//     | Many of 'a node list;;
// type 'a node = One of 'a | Many of 'a node list
type Node<'a> = 
    | One of 'a                // Flatterned case
    | Many of Node<'a> list    // Nested case

let l1 = One 3  //simple list:  One 3
let l2 = Many [l1; One 5]  // one level nested:  Many [One 3; One 5]
let l3 = Many [l2]  // two levels nested:  Many [Many [One 3; One 5]]

// # flattern [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
// - : string list = ["a"; "b"; "c"; "d"; "e"]
let flattern (data: Node<'a> list): 'a list =
    let rec aux arr (data: Node<'a> list): 'a list = 
        match data with
        | [] -> arr
        | head::tail ->
            match head with 
            | One v -> aux (v::arr) tail
            | Many l -> aux (aux arr l) tail
    aux [] data |> List.rev

let flattern2 (data: Node<'a> list): 'a list =
    let rec aux arr (data: Node<'a> list): 'a list =
        match data with 
        | [] -> arr
        | One v::tail -> aux (v::arr) tail
        | Many l::tail -> aux (aux arr l) tail
    aux [] data |> List.rev


// rahul.ratnagiri [5:41 PM] 
// suggest you start with this: https://ocaml.org/learn/tutorials/99problems.html 
// (you can google for f# answers to verify)

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


// So I know that `::` will prepend an item to a list, e.g. `'a' :: ['b'; 'c']`; 
// is there a symbol in F# that append the item to a list ?
// e.g. `['b'; 'c'] ??? 'e'` 
// rahul.ratnagiri [May 10]
// @ying reverse and prepend if you want to do it tail-recursively
// ruben [May 10] 
// a common technique is to accumulate with prepends and then do a `List.rev` at the end, 
// which under the covers is _relatively_ efficient and you’re only doing a single junk generation
// step per list vs doing it per item if you append
let rec reverse' acc aList =
    match aList with
    | [] -> acc
    | head :: tail -> reverse' (head::acc) tail 

//Reverse a list. (easy)
let reverse aList = 
    let rec aux acc aList =
        match aList with
        | [] -> acc
        | head :: tail -> aux (head::acc) tail 
    aux [] aList

// Find out whether a list is a palindrome. (easy)
// HINT: a palindrome is its own reverse.
let rec isPalindrome aList = 
    reverse aList = aList


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

//Avoid nested match
let flattern2 (data: Node<'a> list): 'a list =
    let rec aux arr (data: Node<'a> list): 'a list =
        match data with 
        | [] -> arr
        | One v::tail -> aux (v::arr) tail
        | Many l::tail -> aux (aux arr l) tail
    aux [] data |> List.rev


// Eliminate consecutive duplicates of list elements. (medium)
// # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
// - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
let compress (data: 'a list): 'a list =
    let rec aux arr (data: 'a list): 'a list =
        match data with
        | [] -> arr
        | head::tail ->
            if List.isEmpty arr then
                aux (head::arr) tail
            elif arr.[0] = head then
                aux arr tail
            else
                aux (head::arr) tail
    aux [] data |> List.rev      

let rec compress2 (data: 'a list): 'a list =
    match data with
    | a::(b::_ as t) -> 
        if a = b then compress2 t
        else a::(compress2 t)
    | l -> l 


// Run-length encoding of a list. (easy)
// If you need so, refresh your memory about run-length encoding.
// Here is an example:
// # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
// - : (int * string) list =
// [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
let encode (data: string list): (int * string) list = 
    let rec aux (acc:int) (data: string list) : (int * string) list =
        match data with 
        | a::(b::_ as t) -> 
            if a = b then 
                aux (acc+1) t
            else
                (acc+1, a)::(aux 0 t)
        | [a] -> [(1, a)] 
        | [] -> [] 
    aux 0 data


// Duplicate the elements of a list. (easy)
// duplicate ["a";"b";"c";"c";"d"];;
// - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
let duplicate (data: 'a list): 'a list =
    let rec aux arr data = 
        match data with 
        | [] -> arr
        | head::tail -> aux (head::head::arr) tail
    aux [] data |> List.rev

let rec duplicate1 (data: 'a list): 'a list =
    match data with 
    | [] -> []
    | head::tail -> head::head::duplicate1 tail


// Replicate the elements of a list a given number of times. (medium) 
// # replicate ["a";"b";"c"] 3;;
// - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
let replicate (data: 'a list) (n:int): 'a list =
    let genN (item:'a) (n:int) =
        [for i in 1..n -> item]
    let rec aux (data: 'a list) arr: ('a list) =
        match data with
        | [] -> arr
        | head::tail ->
            aux tail ((genN head n) @ arr)  //Not ideal
    aux data [] |> List.rev

/// this one perform slightly better since it uses prepend
let replicate1 (data: 'a list) (n: int): 'a list =
    let rec prependN (item: 'a) (n:int) (arr: 'a list) : 'a list =
        if n = 0 then
            arr
        else
            prependN item (n-1) (item::arr)
    let rec aux (data: 'a list) arr: 'a list = 
        match data with
        | [] -> arr
        | head::tail ->
            aux tail (prependN head n arr)
    aux data [] |> List.rev


// Drop every N'th element from a list. (medium)
// # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
// - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
let drop (data: 'a list) (n:int): 'a list =
    let rec aux (data: 'a list) (count:int): 'a list =
        match data with
        | [] -> []
        | head::tail ->
            if count = n then
                aux tail 1
            else
                head::(aux tail (count + 1))
    aux data 1


// Split a list into two parts; the length of the first part is given. (easy)
// If the length of the first part is longer than the entire list, then the first part is the list and 
// the second part is empty.

// split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
// - : string list * string list =
// (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
// split ["a";"b";"c";"d"] 5;;
// string list * string list = (["a"; "b"; "c"; "d"], [])

let split (data: 'a list) (n:int): ('a list * 'a list) =
    let rec aux (data: 'a list) (count:int) (acc: 'a list) = 
        match data with 
        | [] -> (acc |> List.rev, [])
        | head::tail -> 
            if count = 1 then
                ((head::acc)|>List.rev, tail)
            else
                aux tail (count-1) (head::acc)
    aux data n []


// Rotate a list N places to the left. (medium)
// # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
// - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
// # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;  [h, g] [f, e, d, c, b, a] -> [g, h] @ [a, b, c, d, e, f]
// - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
let rotate (data: 'a list) (n:int): 'a list =
    //todo use split to solve it ?
    if n > 0 then
        let a, b = split data n
        b @ a
    else
        let a, b = split (List.rev data) (-n)
        (a |> List.rev) @ (b |> List.rev)


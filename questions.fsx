
// Jose's interview question 2018-1-5
// find increasing contigious ranges in an int array.
// intput : [1; 2; 3; 5; 7; 8; 10] 
// output:  1-3, 5, 7-8, 10

/// idea: the point of termines the block is at: when the current item is not part of the (first,last) range
/// use range: (int*int) list as storage passed along
/// first, last are the previous range block
let findContigiousRange(numbers: int list): string list =
    let rec find(numbers: int list)(first: int)(last: int)(range: (int*int) list) =
        match numbers with
        | head::tail ->
            if head = last + 1 then //contigious     
                find tail first head range
            else  //end of the range block
                find tail head head ((first,last)::range)
        | [] ->
            (first,last)::range

    let first = numbers |> List.head
    find (numbers |> List.skip 1) first first []
    |> List.rev
    |> List.map (fun c ->
                    let a, b = c
                    if a = b then
                        sprintf "%d" a
                    else 
                        sprintf "%d-%d" a b) 




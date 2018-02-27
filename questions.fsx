open System
//open System.Collections.Generic


/// 2-25-2018 
/// Jet Interview question James Wang
/// input: a list of tuples(flight tickets) contains (source,destination)
/// output: a list of tuples in the sequence of visiting a valid solution
/// Given a list of tickets, find a valid solution that: 
/// 1) used all the tickets 
/// 2) the first ticket start from JFK
/// assume there is always a valid solution in the given list; assume no duplicate tickets
/// example: [(JFK, SFO), (JFK, AIL), (SFO, AIL), (AIL, SFO), (AIL, JFK)]
/// case 1: [(JFK, SFO), (SFO, AIL), (AIL, SFO)] //dead-end. 
/// case 2: [(JFK, AIL), (AIL, SFO), (SFO, AIL), (AIL, JFK), (JFK, SFO)] //finish

/// idea: needs backtrack: recursion can backtrack. 
/// it is indeed a DFS


/// DIDN't solve. try find an easier graph DFS problem and solve it later. 
let findRoutes(tickets: (string*string) list)=
    // return: (string*string) list 
    // a Map that key = src, and value = Set of dests
    let ticketsSet =
        tickets
        |> List.fold (fun (s:Map<string, string Set>) (src, dest) ->
                            match s.ContainsKey src with 
                            | false ->
                                s.Add (src, [dest]|> Set)
                            | true ->
                                let destSet = s.[src]
                                s.Add (src, destSet.Add dest)
                        ) Map.empty
    
    let removeTicket (tickets: Map<string, string Set>) (src, dest) =
        let dests = tickets.[src]
        match dests.Count with
        | 1 -> tickets.Remove src
        | _ -> tickets.Add (src, (dests.Remove dest))


    /// understand fold: DFS https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-2b/
    let rec aux (tickets: Map<string, string Set>) (path: (string*string) list) (src: string) =
        //base case: no more tickets, return path
        match tickets.Count with
        | 0 -> path
        | _ -> // has more tickets to use
            for dest in tickets.[src] do
                match tickets.ContainsKey dest with
                | false -> //didn't not find
                    ("DEAD", "DEAD")::path
                | true ->
                    let tickets = removeTicket tickets (src, dest)
                    aux tickets ((src,dest)::path) dest
            
    
    aux ticketsSet List.empty "JFK"       

//findRoutes [("JFK", "SFO"); ("JFK", "AIL"); ("SFO", "AIL"); ("AIL", "SFO"); ("AIL", "JFK")] ;;



let rec myMap (f: 'T -> 'U) (data: 'T list): 'U list =
    match data with
    | head :: tail ->  (f head) :: (myMap f tail)
    | [] -> []

// Programming interview exposed Chapter 6, Arrays practice 
// 	 * 
// 	 * Given a array of String, find first the first non-repeated 
// 	 * char and return it. e.g. given "ababec", return e. 
// 	 * use O(n) efficiency. 
let nonRepeatingChar (str:string) =
    //Note: key point is use two passes. first one to build the Map<char, int>; second pass to find the first <char, 1> 
    let lookup = Seq.fold (fun (s:Map<char, int>) (x:char) ->
                            match s.ContainsKey x with
                            | true ->
                                s.Add (x, s.[x] + 1)
                            | false ->
                                s.Add (x, 1)        
                        ) Map.empty str
    Seq.tryFind (fun x-> lookup.[x] = 1) str

// Jose's interview question: implement a fold function
let rec myFold(f: 'S -> 'T -> 'S) (initial: 'S) (data: 'T list): 'S = 
    match data with
    | head::tail -> myFold f (f initial head) tail  
    | [] -> initial

/// given a string, verify that it contains balanced parentheses.
let rec balancedParen(str: char list) (openState: bool) =
    match str with
    | head::tail ->
        if not openState then
            if head = '(' then
                balancedParen tail true
            else
                balancedParen tail false   
        else
            if head = ')' then
                true
            else
                balancedParen tail true
    | [] -> false






// Hugh Richardson's interview at Jet. I shadowed it 
// given input: ["cat", "dog", "act"]
// output the groups of anagrams: [["cat", "act"], ["dog"]

// given input: ["cat", "dog", "act"]
// output the groups of anagrams: [["cat", "act"], ["dog"]
let groupByAnagram(words: string list): string list list =
    words
    |> List.map (fun x -> (x|>Seq.sort|>String.Concat, x))
    |> List.fold (fun (s: Map<string, Set<string>>) (sortedWord, word) ->
            let wordsOpt = s.TryFind sortedWord
            match wordsOpt with
            | Some words when words.Count > 0 ->
                s.Add (sortedWord, (words.Add word))
            | _ ->
                let emptySet = Set.empty
                s.Add (sortedWord, emptySet.Add word)
        ) Map.empty
    |> Seq.map (fun kvp ->
            kvp.Value |> Seq.toList
        )
    |> Seq.toList


let groupByAnagram1(words: string list): string list list =
    let sortStr (word:string): string =
        word
        |> Seq.sort
        |> String.Concat
    words
    |> List.map (fun word -> (word|>sortStr, word))
    |> List.groupBy (fun (k, _) -> k)
    |> List.map (snd >> (List.map snd))


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
    find (numbers |> List.tail) first first []
    |> List.rev
    |> List.map (fun c ->
                    let a, b = c
                    if a = b then
                        sprintf "%d" a
                    else 
                        sprintf "%d-%d" a b) 



(*
A code question RiverBed asked during the Oct-8-2010 Job Fair at Stony Brook. 
* Given a String "qaqiaoo". A duplicate is defined in that, whenever a character is 
* repeated second time, then it is a duplicates. e.g. the second char 'q'in the 
* string is a duplicate. give a code to output the duplicates of the string. e.g.
* "qao", the result could be any permutation. 
*)
let getdups (str: string): string = 
    str
    |> Seq.fold (fun (has_seen:Set<char>, out:Set<char>) c ->
            if has_seen.Contains c then
                (has_seen, out.Add c)
            else
                (has_seen.Add c, out)
        ) (Set.empty, Set.empty)
    |> snd
    |> String.Concat

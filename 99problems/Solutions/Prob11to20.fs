module Prob11to20

(**
P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element
    has no duplicates it is simply copied into the result list.
    Only elements with duplicates are transferred as (N E) lists.

    Example:
    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))
*)

type EncItem<'a> =
    | Raw of 'a
    | Enc of int * 'a

let encodeModified lst =
    let encodeItem (count, item) =
        if count = 1
        then Raw item
        else if count > 1
        then Enc(count, item)
        else failwith "count must be a positive integer"

    lst |> Prob01to10.encode |> List.map encodeItem

(**
P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P11.
    Construct its uncompressed version.
*)

let decode lst =
    // convert envoded item into a decoded list of raw items
    let decodeItem =
        function
        | Raw item -> [ item ]
        | Enc (count, item) -> List.replicate count item

    // decode an item and append to a list
    let decodeAppend acc item = (decodeItem item) @ acc

    lst |> List.fold decodeAppend [] |> List.rev

(**
P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly.
    I.e. don't explicitly create the sublists containing the duplicates,
    as in problem P09, but only count them.
    As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

    Example:
    * (encode-direct '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))
*)

let encodeDirect lst =
    // Add item to the encoding in the list's head if encoding is identical to item
    // or create new head encoding.
    let encodeItem acc item =
        let incrementOrNew count prev tail =
            if prev = item then Enc(count + 1, prev) :: tail else (Raw item) :: acc

        match acc with
        | [] -> [ Raw item ]
        | Raw prev :: tail -> incrementOrNew 1 prev tail
        | Enc (count, prev) :: tail -> incrementOrNew count prev tail

    lst |> List.fold encodeItem [] |> List.rev

(**
P14 (*) Duplicate the elements of a list.
    Example:
    * (dupli '(a b c c d))
    (A A B B C C C C D D)
*)

let dupli lst =
    lst
    |> List.fold (fun acc item -> item :: item :: acc) []
    |> List.rev

(**
P15 (**) Replicate the elements of a list a given number of times.
    Example:
    * (repli '(a b c) 3)
    (A A A B B B C C C)
*)

let repli lst n =
    let rec addItemNTimes n acc item =
        if n = 0 then acc else addItemNTimes (n - 1) (item :: acc) item

    lst |> List.fold (addItemNTimes n) [] |> List.rev

(**
P16 (**) Drop every N'th element from a list.
    Example:
    * (drop '(a b c d e f g h i k) 3)
    (A B D E G H K)
*)

let drop lst n =
    let dropCounter (counter, acc) item =
        if counter = 1 then (n, acc) else (counter - 1, item :: acc)

    let (_, res) = List.fold dropCounter (n, []) lst
    List.rev res

(**
P17 (*) Split a list into two parts; the length of the first part is given.
    Do not use any predefined predicates.

    Example:
    * (split '(a b c d e f g h i k) 3)
    ( (A B C) (D E F G H I K))
*)

let split lst num =
    let rec moveOneItem acc lst num =
        if num = 0 then
            [ List.rev acc; lst ]
        else
            match lst with
            | [] -> [ List.rev acc; lst ]
            | x :: xs -> moveOneItem (x :: acc) xs (num - 1)

    moveOneItem [] lst num

let split' lst num =
    let a, b = List.splitAt num lst
    [ a; b ]

let splitArray (arr: 'a []) num = [| arr.[..num - 1]; arr.[num..] |]

let splitArray' arr num =
    let a, b = Array.splitAt num arr
    [| a; b |]

(**
P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

    Example:
    * (slice '(a b c d e f g h i k) 3 7)
    (C D E F G)
*)

// slice the list as [a; b] (boundaries included)
let slice lst a b =
    let rec trimHead a lst =
        match lst with
        | [] -> lst
        | _ :: xs -> if a = 1 then lst else trimHead (a - 1) xs

    let rec trimTail ba acc lst =
        match lst with
        | [] -> acc
        | x :: xs -> if ba < 0 then acc else trimTail (ba - 1) (x :: acc) xs

    lst
    |> trimHead a
    |> trimTail (b - a) []
    |> List.rev

(**
P19 (**) Rotate a list N places to the left.
    Examples:
    * (rotate '(a b c d e f g h) 3)
    (D E F G H A B C)

    * (rotate '(a b c d e f g h) -2)
    (G H A B C D E F)

    Hint: Use the predefined functions length and append, as well as the result of problem P17.
*)

let rotate lst num =
    // take `num` first items from the list and append them to the back
    // `num` is a non-negative integer
    let splitAndMerge num lst =
        let firstPart, secondPart = List.splitAt num lst
        secondPart @ firstPart

    splitAndMerge (if num >= 0 then num else (List.length lst) + num) lst

(**
P20 (*) Remove the K'th element from a list.
    Example:
    * (remove-at '(a b c d) 2)
    (A C D)
*)

// non-tail recursive implementation
// O(n) comp.; O(n) mem. + O(n) stack
// `pos` is a positive integer
let rec removeAt lst pos =
    match lst with
    | [] -> []
    | x :: xs -> if pos = 1 then xs else x :: removeAt xs (pos - 1)

// tail-recursive implementation
// O(n) comp.; O(n) mem.
// `pos` is a positive integer
let removeAt' lst pos =
    // [ b; a ] [ c; d ] -> [ a; b; c; d ]
    let rec revAppend fromLst toLst =
        match fromLst with
        | [] -> toLst
        | x :: xs -> revAppend xs (x :: toLst)

    let rec cutAndJoin pos lst acc =
        match lst with
        | [] -> List.rev acc
        | x :: xs -> if pos = 1 then revAppend acc xs else cutAndJoin (pos - 1) xs (x :: acc)

    cutAndJoin pos lst []

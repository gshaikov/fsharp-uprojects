module Prob01to10

(**
P01 (*) Find the last box of a list.
    Example:
    * (my-last '(a b c d))
    (D)
*)

let rec myLast =
    function
    | [] -> failwith "lst must contain at least one item"
    | [ x ] -> [ x ]
    | _ :: xs -> myLast xs

let myLast' lst =
    match List.tryLast lst with
    | Some x -> [ x ]
    | None -> failwith "lst must contain at least one item"

let myLastInArray arr =
    match Array.tryLast arr with
    | Some x -> [| x |]
    | None -> failwith "arr must contain at least one item"

(**
P02 (*) Find the last but one box of a list.
    Example:
    * (my-but-last '(a b c d))
    (C D)
*)

let rec myButLast =
    function
    | []
    | [ _ ] -> failwith "lst must contain at least two items"
    | [ _; _ ] as ans -> ans
    | _ :: xs -> myButLast xs

let myButLastArray arr =
    match arr with
    | [||]
    | [| _ |] -> failwith "arr must contain at least two items"
    | _ ->
        let len = Array.length arr
        Array.sub arr (len - 2) 2

(**
P03 (*) Find the K'th element of a list.
    The first element in the list is number 1.
    Example:
    * (element-at '(a b c d e) 3)
    C
*)

let rec elementAt lst n =
    match lst, n with
    | _, 0u -> failwith "n must be a positive integer"
    | [], _ -> failwith "n is larger than lst size"
    | x :: _, 1u -> x
    | _ :: xs, n -> elementAt xs (n - 1u)

let elementAt' lst n =
    match List.tryItem (n - 1) lst with
    | Some x -> x
    | None -> failwith "n is larger than lst size"

let elementAtArray arr n =
    if Array.length arr >= n then arr.[n - 1] else failwith "n is larger than arr size"

(**
P04 (*) Find the number of elements of a list.
*)

let numberOfElements lst =
    let rec tailRec lst acc =
        match lst with
        | [] -> acc
        | _ :: xs -> tailRec xs (acc + 1)

    tailRec lst 0

(**
P05 (*) Reverse a list.
*)

let reverseList lst =
    let rec tailRec lst reversed =
        match lst with
        | [] -> reversed
        | x :: xs -> tailRec xs (x :: reversed)

    tailRec lst []

(**
P06 (*) Find out whether a list is a palindrome.
    A palindrome can be read forward or backward; e.g. (x a m a x).
*)

let isPalindrome lst =
    // compare lst and lst' element by element
    // invariant: lst and lst' are the same size
    let rec compare lst lst' =
        match lst, lst' with
        | [], [] -> true
        | x :: xs, x' :: xs' -> if x = x' then compare xs xs' else false
        | _ -> failwithf "lists are not the same size: %A and %A" lst lst'

    reverseList lst |> compare lst

let isPalindrome' lst = List.rev lst |> (=) lst


(**
P07 (**) Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a `flat`
    list by replacing each list with its elements (recursively).

    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Hint: Use the predefined functions list and append.
*)

type NestedList<'a> =
    | Null
    | Cons of 'a * NestedList<'a>
    | LCons of NestedList<'a> * NestedList<'a>

let rec myFlatten lst =
    match lst with
    | Null -> []
    | Cons (x, xs) -> x :: myFlatten xs
    | LCons (lst', xs) -> myFlatten lst' @ myFlatten xs

let myFlattenTailRecursive lst =

    let rec checkList lst acc todo =
        match lst with
        | Null -> checkTodo acc todo
        | Cons (x, xs) -> checkList xs (x :: acc) todo
        | LCons (lst', xs) -> checkList lst' acc (xs :: todo)

    and checkTodo acc todo =
        match todo with
        | [] -> acc
        | x :: xs -> checkList x acc xs

    checkList lst [] [] |> List.rev

(**
P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    Example:
    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)
*)

let compress lst =
    // add an item to the new list if it is different from the list's head
    let filterWithState acc item =
        match List.tryHead acc with
        | None -> [ item ]
        | Some x -> if x = item then acc else item :: acc

    List.fold filterWithState [] lst |> List.rev

(**
P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:
    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))
*)

let pack lst =
    // add item to the head block if its elements are equal to the item;
    // create a new block with an item otherwise.
    let addOrNewBlock acc item =
        match acc with
        | [] -> [ [ item ] ]
        | (x :: xs) as head :: rest -> if x = item then (x :: head) :: rest else [ item ] :: acc
        | [ [] ]
        | [] :: _ -> failwith "illegal"

    List.fold addOrNewBlock [] lst |> List.rev

(**
P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data
    compression method. Consecutive duplicates of elements are encoded as lists (N E)
    where N is the number of duplicates of the element E.

    Example:
    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
*)

// I will use tuples to encode (N E) lists as they seem more appropriate here

let encode lst =
    lst
    |> pack
    |> List.map (fun block -> List.length block, List.head block)

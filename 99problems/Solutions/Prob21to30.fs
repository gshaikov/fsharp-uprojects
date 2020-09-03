module Prob21to30

(**
P21 (*) Insert an element at a given position into a list.
    Example:
    * (insert-at 'alfa '(a b c d) 2)
    (A ALFA B C D)
*)

// invariant: *pos* is >= 1
let insertAt item lst pos =
    let rec attachBack acc lst =
        match acc with
        | [] -> lst
        | x :: xs -> attachBack xs (x :: lst)

    let rec detatchAndInsert acc lst pos =
        match lst, pos with
        | [], 1 -> attachBack acc [ item ]
        | [], _ -> failwith "insert index too large"
        | x :: xs, 1 -> attachBack acc (item :: lst)
        | x :: xs, _ -> detatchAndInsert (x :: acc) xs (pos - 1)

    detatchAndInsert [] lst pos

let insertAt' item lst pos =
    let a, b = List.splitAt (pos - 1) lst
    a @ (item :: b)

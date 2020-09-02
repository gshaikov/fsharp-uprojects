module Tries.NaiveTrie

type Key = string
type KeyAtom = char
type Value<'a> = Option<'a>

type Trie<'a> =
    | Empty
    | Branch of Value<'a> * (KeyAtom * Trie<'a>) list

let lookup trie key =
    let rec processNode trie key =
        match Seq.tryHead key, trie with
        | _, Empty -> None
        | None, Branch (value, _) -> value
        | Some keyAtom, Branch (_, children) -> Seq.tail key |> processChildren children keyAtom

    and processChildren lon keyAtom keyRemaining =
        match lon with
        | [] -> None
        | (_, Empty) :: rest -> None
        | (atom, nextTrie) :: rest ->
            if keyAtom = atom
            then processNode nextTrie keyRemaining
            else processChildren rest keyAtom keyRemaining

    processNode trie key

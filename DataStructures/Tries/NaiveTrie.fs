module Tries.NaiveTrie

type Prefix = byte
type Mask = byte

type Trie =
    | Branch of Prefix * Mask * Trie * Trie
    | Leaf of Prefix * int
    | Empty

let lookup key trie = Some 0

// Radix trie with r = 2, aka PATRICIA tree.
//
// Used as a space-efficient functional key-value storage
// with O(n) lookup where n is the length of the key.
//
// Reference:
// PATRICIA—Practical Algorithm To Retrieve Information Coded in Alphanumeric
// https://dl.acm.org/doi/10.1145/321479.321481
// https://www.youtube.com/watch?v=0udjkEiCjog

module Tries.RadixTrie

// Key is first part in key-value pair
type Key = byte

// Prefix is some part of a key starting from the left
type Prefix = byte

// ControlBit of the branch is a bit on which a decision
// to go left or right is taken on that branch.
// Invariant: contains only one 1, the rest of the bits are 0.
type ControlBit = byte

type IntTrie<'a> =
    | Branch of Prefix * ControlBit * IntTrie<'a> * IntTrie<'a>
    | Leaf of Key * 'a
    | Empty

// Given a *key*, return a preifx of that key such that
// all bits to the left of the control bit in *control*
// are identical the ones in the key, and the rest of the bits
// are zero.
// Example:
//  key: 1001 1110
//  control: 0000 1000
//  return: 1001 0000
let getPrefix key control = key &&& (~~~((control <<< 1) - 1uy))

let rec lookup trie key =
    match trie with
    | Empty -> None
    | Leaf (keyLeaf, value) -> if keyLeaf = key then Some(value) else None
    | Branch (prefixBranch, control, left, right) ->
        if getPrefix key control <> prefixBranch then None
        else if key &&& control = 0uy then lookup left key
        else lookup right key

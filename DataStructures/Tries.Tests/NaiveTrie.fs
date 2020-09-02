namespace Tries.Tests.NaiveTrie

open Xunit
open Tries.Tests.Helpers
open Tries.NaiveTrie

module Lookup =

    module Empty =

        let trie = Empty

        [<Fact>]
        let ``not found`` () = lookup trie "abc" |> StrictEqual None

    module OneNode =

        let trie = Branch(Some 42, [])

        let testCases: obj [] seq =
            seq {
                yield [| ""; Some 42 |]
                yield [| "a"; None |]
                yield [| "ab"; None |]
            }

        [<Theory; MemberData("testCases")>]
        let ``one node`` (key: Key, value: Value<int>) = lookup trie key |> StrictEqual value

    module ThreeNodes =

        let trieTwoLevels =
            Branch
                (Some(99),
                 [ ('a', Branch(Some 42, []))
                   ('b', Branch(Some 18, [])) ])

        let testCasesTwoLevels: obj [] seq =
            seq {
                yield [| ""; Some(99) |]
                yield [| "a"; Some(42) |]
                yield [| "b"; Some(18) |]
                yield [| "ab"; None |]
            }

        [<Theory; MemberData("testCasesTwoLevels")>]
        let ``two levels`` (key: Key, value: Value<int>) =
            lookup trieTwoLevels key |> StrictEqual value

        let trieThreeLevels =
            Branch(Some(99), [ ('a', Branch(Some 42, [ ('b', Branch(Some 18, [])) ])) ])

        let testCasesThreeLevels: obj [] seq =
            seq {
                yield [| ""; Some(99) |]
                yield [| "a"; Some(42) |]
                yield [| "b"; None |]
                yield [| "ab"; Some(18) |]
                yield [| "abc"; None |]
            }

        [<Theory; MemberData("testCasesThreeLevels")>]
        let ``three levels`` (key: Key, value: Value<int>) =
            lookup trieThreeLevels key |> StrictEqual value

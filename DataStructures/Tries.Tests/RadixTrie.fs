namespace Tries.Tests.RadixTrie

open Xunit
open Tries.Tests.Helpers
open Tries.RadixTrie

module GetPrefix =

    [<Theory>]
    [<InlineData(0uy, 0uy)>]
    [<InlineData(1uy, 0uy)>]
    [<InlineData(16uy, 16uy)>]
    [<InlineData(255uy, 254uy)>]
    let ``control low bit`` (key: Key, result: Prefix) = getPrefix key 1uy |> StrictEqual result

    [<Theory>]
    [<InlineData(0uy, 0uy)>]
    [<InlineData(1uy, 0uy)>]
    [<InlineData(16uy, 0uy)>]
    [<InlineData(255uy, 224uy)>]
    let ``control middle bit`` (key: Key, result: Prefix) = getPrefix key 16uy |> StrictEqual result

    [<Theory>]
    [<InlineData(0uy, 0uy)>]
    [<InlineData(1uy, 0uy)>]
    [<InlineData(16uy, 0uy)>]
    [<InlineData(255uy, 0uy)>]
    let ``control high bit`` (key: Key, result: Prefix) =
        getPrefix key 128uy |> StrictEqual result

module Lookup =

    module Empty =

        let trie = Empty

        [<Fact>]
        let ``not found`` () = lookup trie 0uy |> StrictEqual None

    module JustLeaf =

        let trie = Leaf(0uy, 42)

        let testCases: obj [] seq =
            seq {
                yield [| 0uy; Some 42 |]
                yield [| 1uy; None |]
                yield [| 255uy; None |]
            }

        [<Theory; MemberData("testCases")>]
        let ``just leaf`` (key: Key, value: Option<int>) = lookup trie key |> StrictEqual value

    module OneBranch =

        let trieKeysClose =
            Branch(0uy, 1uy, Leaf(0uy, 42), Leaf(1uy, 18))

        let testCasesKeysClose: obj [] seq =
            seq {
                yield [| 0uy; Some 42 |]
                yield [| 1uy; Some 18 |]
                yield [| 255uy; None |]
            }

        [<Theory; MemberData("testCasesKeysClose")>]
        let ``one branch keys close`` (key: Key, value: Option<int>) =
            lookup trieKeysClose key |> StrictEqual value


        // 0uy      == 0000 0000
        // 255uy    == 1111 1111
        // 0uy      == 0000 0000 -- prefix
        // 128uy    == 1000 0000 -- control bit
        let trieKeysApart =
            Branch(0uy, 128uy, Leaf(0uy, 42), Leaf(255uy, 18))

        let testCasesKeysApart: obj [] seq =
            seq {
                yield [| 0uy; Some 42 |]
                yield [| 1uy; None |]
                yield [| 255uy; Some 18 |]
            }

        [<Theory; MemberData("testCasesKeysApart")>]
        let ``one branch keys apart`` (key: Key, value: Option<int>) =
            lookup trieKeysApart key |> StrictEqual value


        // 17uy     == 0001 0001 -- 42
        // 25uy     == 0001 1001 -- 18
        // 16uy     == 0001 0000 -- prefix
        // 8uy      == 0000 1000 -- control bit
        // 0uy      == 0000 0000 -- compressed away
        // 128uy    == 1000 0000 -- compressed away
        let trieBranchMiddle =
            Branch(16uy, 8uy, Leaf(17uy, 42), Leaf(25uy, 18))

        let testCasesBranchMiddle: obj [] seq =
            seq {
                yield [| 17uy; Some 42 |]
                yield [| 25uy; Some 18 |]
                yield [| 0uy; None |]
                yield [| 128uy; None |]
            }

        [<Theory; MemberData("testCasesBranchMiddle")>]
        let ``one branch branch middle`` (key: Key, value: Option<int>) =
            lookup trieBranchMiddle key |> StrictEqual value

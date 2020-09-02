namespace Tries.Tests.NaiveTrie

open Xunit
open Tries.Tests.Helpers
open Tries.NaiveTrie

module Lookup =

    module Empty =

        let trie = Empty

        [<Fact>]
        let ``not found`` () = lookup 0uy trie |> StrictEqual None

    module Onelevel =

        let trie = Leaf(15uy, 42)

        [<Fact>]
        let ``not found`` () = lookup 0uy trie |> StrictEqual None

        [<Fact>]
        let ``found 42`` () = lookup 15uy trie |> StrictEqual(Some 42)

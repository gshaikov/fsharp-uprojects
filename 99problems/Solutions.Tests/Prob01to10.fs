namespace Prob01to10.Test

open Xunit
open Prob01to10

module P01 =

    [<Fact>]
    let ``four items`` () =
        myLast [ 1; 2; 3; 4 ] |> FAssert.StrictEqual [ 4 ]

    [<Fact>]
    let ``four items alt`` () =
        myLast' [ 1; 2; 3; 4 ]
        |> FAssert.StrictEqual [ 4 ]

    [<Fact>]
    let ``four items array`` () =
        myLastInArray [| 1; 2; 3; 4 |]
        |> FAssert.StrictEqualArrays [| 4 |]

module P02 =

    [<Fact>]
    let ``four items`` () =
        myButLast [ 1; 2; 3; 4 ]
        |> FAssert.StrictEqual [ 3; 4 ]

    [<Fact>]
    let ``four items array`` () =
        myButLastArray [| 1; 2; 3; 4 |]
        |> FAssert.StrictEqualArrays [| 3; 4 |]

module P03 =

    [<Fact>]
    let ``third item`` () =
        elementAt [ 1; 2; 3; 4; 5 ] 3u
        |> FAssert.StrictEqual 3

    [<Fact>]
    let ``third item alt`` () =
        elementAt' [ 1; 2; 3; 4; 5 ] 3
        |> FAssert.StrictEqual 3

    [<Fact>]
    let ``third item array`` () =
        elementAtArray [| 1; 2; 3; 4; 5 |] 3
        |> FAssert.StrictEqual 3

module P04 =

    [<Fact>]
    let ``five items`` () =
        numberOfElements [ 1; 2; 3; 4; 5 ]
        |> FAssert.StrictEqual 5

module P05 =

    [<Fact>]
    let ``five items`` () =
        reverseList [ 1; 2; 3; 4; 5 ]
        |> FAssert.StrictEqual [ 5; 4; 3; 2; 1 ]

module P06 =

    [<Fact>]
    let ``valid palindromes`` () =
        isPalindrome [ 1; 2; 3; 2; 1 ]
        |> FAssert.StrictEqual true
        isPalindrome [ 1; 2; 2; 1 ]
        |> FAssert.StrictEqual true

    [<Fact>]
    let ``valid palindromes alt`` () =
        isPalindrome' [ 1; 2; 3; 2; 1 ]
        |> FAssert.StrictEqual true
        isPalindrome' [ 1; 2; 2; 1 ]
        |> FAssert.StrictEqual true

    [<Fact>]
    let ``not palindromes`` () =
        isPalindrome [ 1; 2; 2; 2 ]
        |> FAssert.StrictEqual false

    [<Fact>]
    let ``not palindromes alt`` () =
        isPalindrome' [ 1; 2; 2; 2 ]
        |> FAssert.StrictEqual false

module P07 =

    [<Fact>]
    let ``one item`` () =
        myFlatten (Cons(1, LCons(Null, Null)))
        |> FAssert.StrictEqual [ 1 ]

    [<Fact>]
    let ``two nested`` () =
        myFlatten (Cons(1, LCons(Cons(2, LCons(Cons(3, Cons(4, Null)), Cons(5, Null))), Null)))
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 5 ]

    [<Fact>]
    let ``tail rec one item`` () =
        myFlattenTailRecursive (Cons(1, LCons(Null, Null)))
        |> FAssert.StrictEqual [ 1 ]

    [<Fact>]
    let ``tail rec two nested`` () =
        myFlattenTailRecursive (Cons(1, LCons(Cons(2, LCons(Cons(3, Cons(4, Null)), Cons(5, Null))), Null)))
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 5 ]

module P08 =

    [<Fact>]
    let ``six blocks`` () =
        compress [ 1; 2; 2; 3; 3; 3; 4; 1; 1; 6; 6; 6 ]
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 1; 6 ]


    [<Fact>]
    let ``six items all unique`` () =
        compress [ 1; 2; 3; 4; 2; 1 ]
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 2; 1 ]

module P09 =

    [<Fact>]
    let ``six packs`` () =
        pack [ 1; 1; 2; 3; 3; 3; 4; 1; 1; 1; 6; 6 ]
        |> FAssert.StrictEqual
            [ [ 1; 1 ]
              [ 2 ]
              [ 3; 3; 3 ]
              [ 4 ]
              [ 1; 1; 1 ]
              [ 6; 6 ] ]


    [<Fact>]
    let ``six items all unique`` () =
        pack [ 1; 2; 3; 4; 2; 1 ]
        |> FAssert.StrictEqual
            [ [ 1 ]
              [ 2 ]
              [ 3 ]
              [ 4 ]
              [ 2 ]
              [ 1 ] ]

module P10 =

    [<Fact>]
    let ``six packs`` () =
        encode [ 1; 1; 2; 3; 3; 3; 4; 1; 1; 1; 6; 6 ]
        |> FAssert.StrictEqual
            [ (2, 1)
              (1, 2)
              (3, 3)
              (1, 4)
              (3, 1)
              (2, 6) ]


    [<Fact>]
    let ``six items all unique`` () =
        encode [ 1; 2; 3; 4; 2; 1 ]
        |> FAssert.StrictEqual
            [ (1, 1)
              (1, 2)
              (1, 3)
              (1, 4)
              (1, 2)
              (1, 1) ]

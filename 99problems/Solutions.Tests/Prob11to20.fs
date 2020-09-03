namespace Prob11to20.Test

open Xunit
open Prob11to20

module P11 =

    [<Fact>]
    let ``three items`` () =
        encodeModified [ 1; 1; 1; 2; 3; 3; 1 ]
        |> FAssert.StrictEqual [ Enc(3, 1); Raw 2; Enc(2, 3); Raw 1 ]

module P12 =

    [<Fact>]
    let ``three items`` () =
        decode [ Enc(3, 1); Raw 2; Enc(2, 3); Raw 1 ]
        |> FAssert.StrictEqual [ 1; 1; 1; 2; 3; 3; 1 ]

module P13 =

    [<Fact>]
    let ``three items`` () =
        encodeDirect [ 1; 1; 1; 2; 3; 3; 1 ]
        |> FAssert.StrictEqual [ Enc(3, 1); Raw 2; Enc(2, 3); Raw 1 ]

module P14 =

    [<Fact>]
    let ``five items`` () =
        dupli [ 1; 2; 3; 3; 4 ]
        |> FAssert.StrictEqual [ 1; 1; 2; 2; 3; 3; 3; 3; 4; 4 ]

module P15 =

    [<Fact>]
    let ``three items duplicate three times`` () =
        repli [ 1; 2; 3 ] 3
        |> FAssert.StrictEqual [ 1; 1; 1; 2; 2; 2; 3; 3; 3 ]

module P16 =

    [<Fact>]
    let ``ten items drop every third`` () =
        drop [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] 3
        |> FAssert.StrictEqual [ 1; 2; 4; 5; 7; 8; 10 ]

module P17 =

    [<Fact>]
    let ``seven items take three`` () =
        split [ 1; 2; 3; 4; 5; 6; 7 ] 3
        |> FAssert.StrictEqual [ [ 1; 2; 3 ]; [ 4; 5; 6; 7 ] ]

    [<Fact>]
    let ``seven items take three alt`` () =
        split' [ 1; 2; 3; 4; 5; 6; 7 ] 3
        |> FAssert.StrictEqual [ [ 1; 2; 3 ]; [ 4; 5; 6; 7 ] ]

    [<Fact>]
    let ``seven items take three array`` () =
        splitArray [| 1; 2; 3; 4; 5; 6; 7 |] 3
        |> FAssert.StrictEqualArrays [| [| 1; 2; 3 |]; [| 4; 5; 6; 7 |] |]

    [<Fact>]
    let ``seven items take three array alt`` () =
        splitArray' [| 1; 2; 3; 4; 5; 6; 7 |] 3
        |> FAssert.StrictEqualArrays [| [| 1; 2; 3 |]; [| 4; 5; 6; 7 |] |]

module P18 =

    [<Fact>]
    let ``zero items 3 4`` () = slice [] 3 4 |> FAssert.StrictEqual []

    [<Fact>]
    let ``four items 3 4`` () =
        slice [ 1; 2; 3; 4 ] 3 4
        |> FAssert.StrictEqual [ 3; 4 ]

    [<Fact>]
    let ``seven items 3 4`` () =
        slice [ 1; 2; 3; 4; 5; 6; 7 ] 3 4
        |> FAssert.StrictEqual [ 3; 4 ]

    [<Fact>]
    let ``seven items 4 4`` () =
        slice [ 1; 2; 3; 4; 5; 6; 7 ] 4 4
        |> FAssert.StrictEqual [ 4 ]

module P19 =

    [<Fact>]
    let ``rotate 3 times forwards`` () =
        rotate [ 1; 2; 3; 4; 5 ] 3
        |> FAssert.StrictEqual [ 4; 5; 1; 2; 3 ]


    [<Fact>]
    let ``rotate 1 times backwards`` () =
        rotate [ 1; 2; 3; 4; 5 ] -1
        |> FAssert.StrictEqual [ 5; 1; 2; 3; 4 ]

    [<Fact>]
    let ``rotate 0 times`` () =
        rotate [ 1; 2; 3; 4; 5 ] 0
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 5 ]

module P20 =

    [<Fact>]
    let ``remove first`` () =
        removeAt [ 1; 2; 3 ] 1
        |> FAssert.StrictEqual [ 2; 3 ]

    [<Fact>]
    let ``remove last`` () =
        removeAt [ 1; 2; 3 ] 3
        |> FAssert.StrictEqual [ 1; 2 ]

    [<Fact>]
    let ``remove too far`` () =
        removeAt [ 1; 2; 3 ] 4
        |> FAssert.StrictEqual [ 1; 2; 3 ]

    [<Fact>]
    let ``remove first tailrec`` () =
        removeAt' [ 1; 2; 3 ] 1
        |> FAssert.StrictEqual [ 2; 3 ]

    [<Fact>]
    let ``remove last tailrec`` () =
        removeAt' [ 1; 2; 3 ] 3
        |> FAssert.StrictEqual [ 1; 2 ]

    [<Fact>]
    let ``remove too far tailrec`` () =
        removeAt' [ 1; 2; 3 ] 4
        |> FAssert.StrictEqual [ 1; 2; 3 ]

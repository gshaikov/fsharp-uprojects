namespace Prob11to20.Test

open Xunit
open Prob21to30

module P21 =

    [<Fact>]
    let ``insert at 1`` () =
        insertAt 99 [ 1; 2; 3; 4 ] 1
        |> FAssert.StrictEqual [ 99; 1; 2; 3; 4 ]

    [<Fact>]
    let ``insert at 5`` () =
        insertAt 99 [ 1; 2; 3; 4 ] 5
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 99 ]


    [<Fact>]
    let ``insert at 1 alt`` () =
        insertAt' 99 [ 1; 2; 3; 4 ] 1
        |> FAssert.StrictEqual [ 99; 1; 2; 3; 4 ]

    [<Fact>]
    let ``insert at 5 alt`` () =
        insertAt' 99 [ 1; 2; 3; 4 ] 5
        |> FAssert.StrictEqual [ 1; 2; 3; 4; 99 ]

module FAssert

open Xunit

let StrictEqual expected result = Assert.StrictEqual(expected, result)

let StrictEqualArrays expected result = (=) expected result |> Assert.True

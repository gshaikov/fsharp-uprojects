module Tries.Tests.Helpers

open Xunit

let Equal expected result = Assert.Equal(expected, result)

let StrictEqual expected result = Assert.StrictEqual(expected, result)

let StrictEqualArrays expected result = (=) expected result |> Assert.True

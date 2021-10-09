module PointRegionQuadtree.Utils

open System             // Math functions
open System.Numerics    // BitOperations function `LeadingZeroCount`

/// Rounding an integer
/// to the next nearest power of 2
let toNextPowerOfTwo num =
    let shift =
        num
        |> float
        |> Math.Log2
        |> Math.Ceiling

    1 <<< int shift

/// Fast rounding an integer
/// to the next nearest power of 2
/// (incompatible with netstandard)
let fastToNextPowerOfTwo (num: int) =
    let shift =
        sizeof<uint> * 8 - BitOperations.LeadingZeroCount(
            (uint num) - 1u)

    int (1u <<< shift)

let shouldBePowerOfTwo num =
    let powerOfTwo = fastToNextPowerOfTwo num
    match num = powerOfTwo with
    | true -> Ok num
    | false -> Error $"should be a power of two; instead got {num}"

let half size =
    float size / 2.0

let checkSize size =
    if size < 1 then failwith $"Size should be >= 1; instead got: {size}"
    match shouldBePowerOfTwo size with
    | Error msg -> failwith $"Size {msg}"
    | Ok _ -> ()



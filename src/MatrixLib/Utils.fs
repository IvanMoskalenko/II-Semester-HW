module MatrixLib.Utils

open System                 // Math functions
open System.Numerics        // BitOperations function `LeadingZeroCount`

/// Rounding an integer to the next nearest power of 2
let toNextPowerOfTwo number =
    let shift =
        number
        |> float
        |> Math.Log2
        |> Math.Ceiling

    1 <<< int shift

/// Fast rounding an integer to the next nearest power of 2 (incompatible with netstandard)
(* WARNING:
    - this function uses `BitOperations.LeadingZeroCount()` (netcore >= 3.1)
    - `System.Numerics.BitOperations` not supported by CLS (incompatible with netstandard) *)
let fastToNextPowerOfTwo (number: int) =
    let shift =
        sizeof<uint> * 8 - BitOperations.LeadingZeroCount(
            (uint number) - 1u)

    int (1u <<< shift)

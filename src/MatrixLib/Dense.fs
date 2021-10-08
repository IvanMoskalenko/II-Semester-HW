module MatrixLib.Dense
open AlgebraicStructures

let sum x y (structure: Semiring<'t>) =
    let a = Array2D.copy x
    if Array2D.length1 x <> Array2D.length1 y || Array2D.length2 x <> Array2D.length2 y
    then failwith "Dimensions of matrices should be equal"
    else
        for i in 0 .. Array2D.length1 x - 1 do
            for j in 0 .. Array2D.length2 x - 1 do
                let res = structure.Addition a.[i, j] y.[i, j]
                a.[i, j] <- res
    a

let multiply (x: 't[,]) (y: 't[,]) structure =
    let row1 = x.[0, *].Length
    let col2 = y.[*, 0].Length
    let result = Array2D.create row1 col2 (structure.GetGenericZero())
    if row1 = col2
    then
        for i = 0 to row1 - 1 do
            for k = 0 to col2 - 1 do
                for r = 0 to row1 - 1 do
                    result.[i, k] <-
                        structure.Addition result.[i, k] (structure.Multiplication x.[i, r] y.[r, k])
        result
    else failwith "Matrices aren't matched"

let multiplyParallel (m1: 't[,]) (m2: 't[,]) structure =
    if m1.GetLength 1 = m2.GetLength 0 then
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.create a c (structure.GetGenericZero())
        [ for i in 0 .. a - 1 ->
            async {
                do
                    for j in 0 .. c - 1 do
                        for k in 0 .. b - 1 do
                            res.[i, j] <- structure.Addition res.[i, j] (structure.Multiplication m1.[i, k] m2.[k, j])
            }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res
    else failwith "It's impossible to multiply matrices of this sizes"


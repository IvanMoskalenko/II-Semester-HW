module MatrixLib.Array2dMatrices
open AlgebraicStructures

[<AutoOpen>]
module MatrixArray2D =
    let add (matrix1: 'a [,]) (matrix2: 'a [,]) structure =
        let sizesAreEqual =
            matrix1.GetLength 0 = matrix2.GetLength 0
            && matrix1.GetLength 1 = matrix2.GetLength 1

        if not sizesAreEqual then
            failwith "Incorrect size of input matrices."

        matrix2
        |> Array2D.iteri (
            fun i j elemMatrix2 ->
                matrix1.[i, j] <- structure.AddOp matrix1.[i, j] elemMatrix2
            )

        matrix1

    let multiply (x: 't[,]) (y: 't[,]) structure =
        let row1 = x.[0, *].Length
        let col2 = y.[*, 0].Length
        let result = Array2D.create row1 col2 (structure.GetZero())
        if row1 = col2
        then
            for i = 0 to row1 - 1 do
                for k = 0 to col2 - 1 do
                    for r = 0 to row1 - 1 do
                        result.[i, k] <-
                            structure.AddOp result.[i, k] (structure.MulOp x.[i, r] y.[r, k])
            result
        else failwith "Matrices aren't matched"

    let multiplyParallel (m1: 't[,]) (m2: 't[,]) structure =
        if m1.GetLength 1 = m2.GetLength 0 then
            let a = m1.GetLength 0
            let b = m1.GetLength 1
            let c = m2.GetLength 1
            let res = Array2D.create a c (structure.GetZero())
            [ for i in 0 .. a - 1 ->
                async {
                    do
                        for j in 0 .. c - 1 do
                            for k in 0 .. b - 1 do
                                res.[i, j] <- structure.AddOp res.[i, j] (structure.MulOp m1.[i, k] m2.[k, j])
                }
            ]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
            res
        else failwith "It's impossible to multiply matrices of this sizes"

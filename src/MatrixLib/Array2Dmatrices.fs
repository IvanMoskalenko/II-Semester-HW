module MatrixLib.Array2dMatrices
open AlgebraicStructures

[<AutoOpen>]
module MatrixArray2D =
    let add (matrix1: 'a [,]) (matrix2: 'a [,]) (structure: Semiring<_>) =
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

    let multiply (x: 't[,]) (y: 't[,]) (structure: Semiring<_>) =
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

    let multiplyParallel n (x: 't[,]) (y: 't[,]) structure =
        let res = Array2D.create (x.GetLength 0) (y.GetLength 1) (structure.GetZero())
        let chunkSize = (x.GetLength 0 - 1) / n
        [ for p in 0 .. n - 1 ->
              async { do
                          for i in p * chunkSize ..  chunkSize * (p + 1) - 1 do
                              for j in 0 .. y.GetLength 1 - 1 do
                                  for k in 0 .. x.GetLength 1 - 1 do
                                      res.[i, j] <- structure.AddOp res.[i, j] (structure.MulOp x.[i, k] y.[k, j])
              }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res

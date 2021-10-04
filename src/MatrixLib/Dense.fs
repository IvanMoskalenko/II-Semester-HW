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

let multiplyParallel n (x: 't[,]) (y: 't[,]) structure =
    let res = Array2D.create (x.GetLength 0) (y.GetLength 1) (structure.GetGenericZero())
    let chunkSize = (x.GetLength 0 - 1) / n
    [ for p in 0 .. n - 1 ->
          async { do
                      for i in p * chunkSize ..  chunkSize * (p + 1) - 1 do
                          for j in 0 .. y.GetLength 1 - 1 do
                              for k in 0 .. x.GetLength 1 - 1 do
                                  res.[i, j] <- structure.Addition res.[i, j] (structure.Multiplication x.[i, k] y.[k, j])
          }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    res


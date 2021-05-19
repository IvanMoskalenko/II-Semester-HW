namespace MatrixLib
open AlgebraicStructures
module DenseOp =
    
    let sum (x: 't[,]) (y: 't[,]) structure =
        let operation = getSumOperation structure
        let a = Array2D.copy x
        if Array2D.length1 x <> Array2D.length1 y || Array2D.length2 x <> Array2D.length2 y
        then failwith "Dimensions of matrices should be equal"
        else
            for i in 0 .. Array2D.length1 x - 1 do
                for j in 0 .. Array2D.length2 x - 1 do
                    let res = operation a.[i, j] y.[i, j]
                    a.[i, j] <- res           
        a
        
    let multiply (x: 't[,]) (y: 't[,]) structure =
        let sumOperation = getSumOperation structure
        let multiplyOperation = getMultiplyOperation structure
        let neutral = getNeutral structure
        let row1 = x.[0, *].Length
        let col2 = y.[*, 0].Length
        let result = Array2D.create row1 col2 neutral
        if row1 = col2
        then
            for i = 0 to row1 - 1 do
                for k = 0 to col2 - 1 do
                    for r = 0 to row1 - 1 do
                        result.[i, k] <- sumOperation result.[i,k] (multiplyOperation x.[i, r]  y.[r, k])
            result
        else failwith "Matrices aren't matched"





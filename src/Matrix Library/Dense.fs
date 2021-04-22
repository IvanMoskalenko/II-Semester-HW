namespace MatrixLib
module Dense =
    let sum x y =
        let a = Array2D.copy x
        if Array2D.length1 x <> Array2D.length1 y || Array2D.length2 x <> Array2D.length2 y
        then failwith "Dimensions of matrices should be equal"
        else
            for i in 0 .. Array2D.length1 x - 1 do
                for j in 0 .. Array2D.length2 x - 1 do
                    a.[i, j] <- a.[i, j] + y.[i, j]
        a
        
    let multiply (x: int[,]) (y: int[,]) =
        let row1 = x.[0, *].Length
        let col2 = y.[*, 0].Length
        let result = Array2D.zeroCreate row1 col2
        if row1 = col2
        then
            for i = 0 to row1 - 1 do
                for k = 0 to col2 - 1 do
                    for r = 0 to row1 - 1 do
                            result.[i, k] <- result.[i, k] + x.[i, r] * y.[r, k]
            result
        else failwith "Matrices aren't matched"       
        
    let scalarMultiply scalar x =
        let y = Array2D.copy x
        for i in 0 .. Array2D.length1 x - 1 do
            for j in 0 .. Array2D.length2 x - 1 do
                y.[i, j] <- scalar * y.[i, j]
        y
    
    let tensorMultiply x y =
        let mutable a, b = 0, 0
        let output = Array2D.create (Array2D.length1 x * Array2D.length2 y) (Array2D.length1 x * Array2D.length2 y) 1
        for i in 0 .. Array2D.length1 x - 1 do
            for j in 0 .. Array2D.length2 x - 1 do
                a <- i * (Array2D.length1 y) 
                b <- j * (Array2D.length2 y)
                Array2D.blit (scalarMultiply x.[i, j] y) 0 0 output a b (Array2D.length1 x) (Array2D.length2 y)
        output




module Generator
open System
open System.IO

type generatorType =
    | Int
    | Float
    | Bool

[<Struct>]
type generatorOptions =
    val rows: int
    val cols: int
    val amt: int
    val sparsity: float
    val path: string
    val bType: generatorType
    new (a, b, c, d, e, f) = { rows = a; cols = b; amt = c; sparsity = d; path = e; bType = f}

let printMatrix (x: string [,]) path =
    let rows = x.GetLength(0)
    let cols = x.GetLength(1)
    use writer = new StreamWriter(path: string)
    for j = 0 to rows - 1 do
        for i = 0 to cols - 1 do
            writer.Write(x.[i, j] + " ")
        writer.WriteLine()

let generateSparseMatrix (x: generatorOptions) =
    let rand = Random()
    for i = 0 to x.amt - 1 do
        let output = Array2D.zeroCreate x.rows x.cols
        for j = 0 to x.rows - 1 do
            for k = 0 to x.cols - 1 do
                let y = rand.NextDouble()
                if y > x.sparsity
                then
                    output.[j, k] <- (match x.bType with
                                      | Int -> string (rand.Next())
                                      | Float -> string (rand.NextDouble() * float Int32.MaxValue)
                                      | Bool -> "1")
                else output.[j, k] <- "0"
        printMatrix output (Path.Combine (x.path, "Matrix" + string i + ".txt"))

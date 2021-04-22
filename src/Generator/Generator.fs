module Generator
open System

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
    let y = x.[*, 1]
    let z = x.[1, *]
    let mutable text = ""
    for i = 0 to y.Length - 1 do
        for j = 0 to z.Length - 1 do
            text <- text + x.[i, j] + " "
        text <- text + "\n"
    IO.File.WriteAllText (path, text)
                
let generator (x: generatorOptions) =  
    for i = 0 to x.amt - 1 do
        let output = Array2D.zeroCreate x.rows x.cols
        for j = 0 to x.rows - 1 do
            for k = 0 to x.cols - 1 do
                let rand = Random()
                let y = rand.NextDouble()
                match x.bType with
                | Int ->
                    if y > x.sparsity   
                    then output.[j, k] <- string (rand.Next())
                    else output.[j, k] <- "0"
                | Float ->
                    if y > x.sparsity   
                    then output.[j, k] <- string (rand.NextDouble() * float Int32.MaxValue) 
                    else output.[j, k] <- "0"
                | Bool ->
                    if y > x.sparsity  
                    then output.[j, k] <- "1" 
                    else output.[j, k] <- "0"
        printMatrix output (IO.Path.Combine (x.path, "Matrix" + string i + ".txt")) 

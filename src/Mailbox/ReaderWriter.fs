module Mailbox.ReaderWriter
open System

let print (x: int [,]) path =
    let rows = x.GetLength(0)
    let cols = x.GetLength(1)
    let mutable text = ""
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            text <- text + string x.[i, j] + " "
        text <- text + "\n"
    IO.File.WriteAllText (path, text)

let read path =
    let fileLines = IO.File.ReadAllLines(path)
    let matrix = Array2D.zeroCreate fileLines.Length (fileLines.[0].Split(' ').Length - 1)
    for i = 0 to fileLines.Length - 1 do
        let line = fileLines.[i].Trim(' ')
        for j = 0 to matrix.GetLength(1) - 1 do
            let split = line.Split(' ')
            matrix.[i, j] <- int split.[j]
    matrix

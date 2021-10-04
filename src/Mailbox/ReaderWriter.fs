module Mailbox. ReaderWriter
open System

let print (x: int [,]) path =
    let y = x.[*, 1]
    let z = x.[1, *]
    let mutable text = ""
    for i = 0 to y.Length - 1 do
        for j = 0 to z.Length - 1 do
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


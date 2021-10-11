module Mailbox.ReaderWriter
open System
open System.IO

let print (x: int [,]) path =
    let rows = x.GetLength(0)
    let cols = x.GetLength(1)
    use writer = new StreamWriter(path: string)
    for j = 0 to rows - 1 do
        for i = 0 to cols - 1 do
            writer.Write(string x.[i, j] + " ");
        writer.WriteLine();

let read path =
    let fileLines = File.ReadAllLines(path)
    let matrix = Array2D.zeroCreate fileLines.Length (fileLines.[0].Split(' ').Length - 1)
    for i = 0 to fileLines.Length - 1 do
        let line = fileLines.[i].Trim(' ')
        for j = 0 to matrix.GetLength(1) - 1 do
            let split = line.Split(' ')
            matrix.[i, j] <- int split.[j]
    matrix

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
    let readLines (path: string) = seq {
        use sr = new StreamReader (path)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    let rawMatrix = readLines path
    let matrix = Array2D.zeroCreate (Seq.length(rawMatrix)) (Seq.head(rawMatrix).Split(' ').Length - 1)
    let writeLine i (line: string) =
        let split = line.Trim(' ').Split(' ')
        for j = 0 to matrix.GetLength(1) - 1 do
            matrix.[i, j] <- int split.[j]
    Seq.iteri writeLine rawMatrix
    matrix

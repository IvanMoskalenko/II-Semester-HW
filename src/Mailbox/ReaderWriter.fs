module Mailbox.ReaderWriter
open System.IO

let print (x: int [,]) path =
    let rows = x.GetLength(0)
    let cols = x.GetLength(1)
    use writer = new StreamWriter(path: string)
    for j = 0 to rows - 1 do
        for i = 0 to cols - 1 do
            writer.Write(string x.[i, j] + " ")
        writer.WriteLine()

let read path =
    File.ReadLines path
    |> Seq.map (fun s -> s.Trim(' ').Split(' ') |> Array.map int)
    |> array2D<_,_>


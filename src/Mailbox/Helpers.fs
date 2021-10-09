module Mailbox.Helpers

type Method =
    | QtParallel
    | QtDefault
    | ArrParallel
    | ArrDefault

type Message =
    | EOS of AsyncReplyChannel<unit>
    | Go of AsyncReplyChannel<unit>
    | PairOfMatrices of ((int[,] * string) * (int[,] * string))

exception NumberOfFilesIsNotEven
let getFilesNames path =
    System.IO.Directory.GetFiles(path) |> List.ofArray

let getAllFilesLength path =
    let length = System.IO.Directory.GetFiles(path).Length
    if length % 2 = 1
    then raise NumberOfFilesIsNotEven
    else length



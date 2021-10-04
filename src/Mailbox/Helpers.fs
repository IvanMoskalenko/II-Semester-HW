module Mailbox.Helpers

type Method =
    | QtParallel
    | QtDefault
    | ArrParallel
    | ArrDefault

type Message =
    | EOS of AsyncReplyChannel<unit>
    | Go of AsyncReplyChannel<unit>
    | PairOfMatrices of int[,] * int[,]

let getFilesName path = System.IO.Directory.GetFiles(path) |> List.ofArray


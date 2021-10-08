module Mailbox.Balancer
open Mailbox.Helpers

let evaluateSparsity (mtx: int[,]) =
    let mutable nons = 0.
    let numOfElems = float <| mtx.GetLength(0) * mtx.GetLength(1)
    for i in 0 .. mtx.GetLength(0) - 1 do
        for j in 0 .. mtx.GetLength(1) - 1 do
            if mtx.[i, j] = 0
            then nons <- nons + 1.
    nons / numOfElems

let tupleRate fst snd =
    let sparsityA = evaluateSparsity fst
    let sparsityB = evaluateSparsity snd
    let totalSize = fst.GetLength(0) + fst.GetLength(1) + snd.GetLength(0) + snd.GetLength(1)
    match (sparsityA, sparsityB, totalSize) with
    | _, _, _ when (sparsityA > 0.75 || sparsityB > 0.75) && (totalSize < 16) -> QtDefault
    | _, _, _ when (sparsityA > 0.75 || sparsityB > 0.75) && (totalSize >= 16) -> QtParallel
    | _, _, _ when (sparsityA <= 0.75 && sparsityB <= 0.75) && (totalSize < 16) -> ArrDefault
    | _, _, _  -> ArrParallel



let balancer (qtMultiply: MailboxProcessor<Message>) (qtParallelMultiply: MailboxProcessor<Message>)
             (arrMultiply: MailboxProcessor<Message>) (arrParallelMultiply: MailboxProcessor<Message>) =

    let eos = qtMultiply.PostAndReply EOS,
              arrMultiply.PostAndReply EOS,
              qtParallelMultiply.PostAndReply EOS,
              arrParallelMultiply.PostAndReply EOS

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    eos |> ignore
                    ch.Reply()

                | PairOfMatrices (fst, snd) as tuple ->
                    match tupleRate fst snd with
                        | QtParallel -> qtParallelMultiply.Post tuple
                        | QtDefault -> qtMultiply.Post tuple
                        | ArrDefault -> arrMultiply.Post tuple
                        | ArrParallel -> arrParallelMultiply.Post tuple

                    return! loop ()

                | _ -> failwith "not balancer's task"
            }
        loop ()
    )

module Mailbox.Loader
open Mailbox.Helpers
open Mailbox.ReaderWriter
open System.IO

let loader path (balancer: MailboxProcessor<messageBalancer>) (pairAmount: int) =

    let mtxList = getFilesNames path

    let input =
        mtxList.[ .. pairAmount * 2 - 1]

    MailboxProcessor.Start(fun inbox ->
        let rec loop input =
            async {
                let! msg = inbox.Receive()
                match msg with
                | messageLoader.EOS ch ->
                    balancer.PostAndReply messageBalancer.EOS
                    ch.Reply()
                | Go ch as msg ->
                    match input with
                    | [] ->
                        inbox.Post (messageLoader.EOS ch)
                        return! loop input
                    | fstName :: sndName :: tail ->
                        let firstMatrix = read fstName
                        let secondMatrix = read sndName
                        ((firstMatrix, Path.GetFileNameWithoutExtension(fstName)),
                         (secondMatrix, Path.GetFileNameWithoutExtension(sndName)))
                        |> PairOfMatrices |> balancer.Post
                        inbox.Post msg
                        return! loop tail
                    | [_] -> failwith "Number of files isn't even"
            }
        loop input
        )


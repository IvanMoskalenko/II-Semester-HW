module Mailbox.Loader
open Mailbox.Helpers
open Mailbox.ReaderWriter


let loader path (balancer: MailboxProcessor<Message>) (pairAmount: int) =

    let mtxList = getFilesName path

    let input =
        mtxList.[ .. pairAmount * 2 - 1]

    MailboxProcessor.Start(fun inbox ->
        let rec loop input =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    balancer.PostAndReply EOS
                    ch.Reply()

                | Go ch as msg ->
                    match input with
                    | [] ->
                        inbox.Post (EOS ch)
                        return! loop input

                    | fst :: snd :: tail ->
                        (read fst, read snd) |> PairOfMatrices |> balancer.Post
                        inbox.Post msg
                        return! loop tail

                    | _ :: _ -> failwith "number of files isn't even"

                | _ -> failwith "not loader's task"
            }
        loop input
        )


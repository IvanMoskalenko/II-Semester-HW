module Mailbox.Multiplier
open Mailbox.Helpers
open Mailbox.ReaderWriter

let multiplier multiplyFun converter deconverter saveDir =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | messageBalancer.EOS ch ->
                    ch.Reply()
                    return! loop ()
                | PairOfMatrices ((fst, fstName), (snd, sndName)) ->
                    let! convertedFirst = converter fst
                    let! convertedSecond = converter snd
                    let! res = multiplyFun convertedFirst convertedSecond
                    let! deconverted = deconverter res
                    do! print deconverted (System.IO.Path.Join (saveDir, $"{fstName}_X_{sndName}.txt"))
                    return! loop ()
            }
        loop ()
    )


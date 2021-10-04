module Mailbox.Multiplier
open Mailbox.Helpers
open Mailbox.ReaderWriter
open System.Threading

let multiplier multiplyFun converter deconverter =
    let mutable counter = 0
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    ch.Reply()
                    return! loop ()

                | PairOfMatrices (fst, snd) ->
                    let res = multiplyFun (converter fst) (converter snd)
                    print (deconverter res) $"/home/ivan/Documents/test2/result{counter}.txt"
                    counter <- Interlocked.Increment(ref counter)
                    return! loop ()

                | _ -> failwith "not multiplier's task"
            }
        loop ()
    )




module Mailbox.Multiplier
open Mailbox.Helpers
open Mailbox.ReaderWriter
open MatrixLib.SparseMatrixQT

let multiplier multiplyFun converter (deconverter: 't -> int[,]) saveDir =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | messageBalancer.EOS ch ->
                    ch.Reply()
                    return! loop ()
                | PairOfMatrices ((fst, fstName), (snd, sndName)) ->
                    let convertedFirst = converter fst
                    let convertedSecond = converter snd
                    let res = multiplyFun convertedFirst convertedSecond
                    let deconverted = (deconverter res).[0..fst.GetLength(0) - 1, 0..snd.GetLength(1) - 1]
                    print deconverted (System.IO.Path.Join (saveDir, $"{fstName}_X_{sndName}.txt"))
                    return! loop ()
            }
        loop ()
    )


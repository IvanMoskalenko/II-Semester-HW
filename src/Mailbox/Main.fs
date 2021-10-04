open Argu
open Worker
open Mailbox.Helpers

    type CLIArguments =
        | MultSome of path: string * amount: int
        | MultAll of path: string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | MultSome _ -> "Multiplies concrete amount of pairs of matrices"
                | MultAll _ -> "Multiplies all pairs of matrices"

    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MatricesMailbox")
            let args = parser.ParseCommandLine argv
            if args.Contains(MultSome)
            then
                let inputPath, amount = args.GetResult(MultSome)
                processFiles inputPath amount

            elif args.Contains(MultAll)
            then
                let inputPath = args.GetResult(MultAll)
                processFiles inputPath ((getFilesName inputPath).Length / 2)
            else printfn "No such mode, sorry"
            0
        with
        | :? ArguParseException as ex ->
            printfn $"%s{ex.Message}"
            1

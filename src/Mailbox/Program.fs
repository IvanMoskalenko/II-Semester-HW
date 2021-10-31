open Argu
open Worker
open Mailbox.Helpers

    type CLIArguments =
        | [<Mandatory>] InputDirectory of path: string
        | [<Mandatory>] SaveDirectory of path: string
        | Amount of amount: int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputDirectory _ -> "Specify the directory with matrices."
                | SaveDirectory _ -> "Specify the directory with output results."
                | Amount _ -> "Specify the number of pairs of matrices."

    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MatricesMailbox")
            let args = parser.ParseCommandLine argv
            let inputPath, saveDir = args.GetResult(InputDirectory), args.GetResult(SaveDirectory)
            if not (System.IO.Directory.Exists(inputPath)) || not (System.IO.Directory.Exists(saveDir))
            then failwith "Invalid directories"
            if args.Contains(Amount)
            then processFiles inputPath (args.GetResult(Amount)) saveDir
            else processFiles inputPath (getAllFilesLength inputPath / 2) saveDir
            0
        with
        | :? NumberOfFilesIsNotEven ->
            printfn "Sorry, but numbers of files in directory is not even."
            1
        | :? ArguParseException as ex ->
            printfn $"%s{ex.Message}"
            1

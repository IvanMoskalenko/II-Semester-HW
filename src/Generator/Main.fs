module Main
open Generator
open Argu

type CliArguments =
    | Rows of rows: int
    | Cols of cols: int
    | Amount of amt: int
    | Sparsity of sparsity: float
    | Path of path: string
    | Type of bType: generatorType
    
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Rows _ -> "Specify the number of rows"
            | Cols _ -> "Specify the number of cols"
            | Amount _ -> "Specify the number of matrices"
            | Sparsity _ -> "Specify the sparsity"
            | Path _ -> "Specify the target directory"
            | Type _ -> "Specify the type of matrices"
            
            
[<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CliArguments> (programName = "Generator")
        try
        let x = parser.Parse argv  
        let y = generatorOptions
                    (x.GetResult Rows, x.GetResult Cols, x.GetResult Amount, x.GetResult Sparsity, x.GetResult Path, x.GetResult Type)            
        generator y
        0    
        with
        | :? ArguParseException as ex ->
            printfn $"%s{ex.Message}"
            1
        | ex ->
            printfn "Internal Error:"
            printfn $"%s{ex.Message}"
            2
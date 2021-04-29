module Main
open System.IO
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
            | Sparsity _ -> "Specify the sparsity as number from 0.0 to 1.0"
            | Path _ -> "Specify the target directory"
            | Type _ -> "Specify the type of matrices"            
            
[<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CliArguments> (programName = "Generator")
        try
        let x = parser.Parse argv
        if x.GetResult Rows <= 0 || x.GetResult Cols <= 0
        then failwith "Number of rows and cols must be positive"
        elif x.GetResult Amount <= 0
        then failwith "Number of matrices must be positive"
        elif x.GetResult Sparsity < 0.0 || x.GetResult Sparsity > 1.0
        then failwith "The sparsity should be defined as a number between 0.0 and 1.0"
        elif not (Directory.Exists (x.GetResult Path))
        then failwith "The specified path does not exist"
        else
            let y = generatorOptions
                        (x.GetResult Rows, x.GetResult Cols, x.GetResult Amount, x.GetResult Sparsity, x.GetResult Path, x.GetResult Type)            
            generateSparseMatrix y
        0    
        with
        | :? ArguParseException as ex ->
            printfn $"%s{ex.Message}"
            1
        | ex ->
            printfn "Internal Error:"
            printfn $"%s{ex.Message}"
            2
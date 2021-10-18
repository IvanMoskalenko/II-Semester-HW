module Tests
open System.IO
open Expecto
open Generator
open Mailbox
open Worker
open ReaderWriter

let tempPath = __SOURCE_DIRECTORY__ + "/tempMatrices"
let tempPathToSave = __SOURCE_DIRECTORY__ + "/tempMatricesResults"

let helper size numberToGenerate numberOfPairsToMultiply =
    let tempPath = __SOURCE_DIRECTORY__ + "/tempMatrices"
    let generatorConfig = generatorOptions(size, size, numberToGenerate, 0.75, tempPath, Int)
    generateSparseMatrix(generatorConfig)
    processFiles tempPath numberOfPairsToMultiply tempPath
    let allFiles = Directory.GetFiles(tempPath)
    for file in allFiles do
        File.Delete(file)
    allFiles.Length


[<Tests>]
let tests =
    testSequenced <| testList "Mailbox tests" [

        testProperty "Number of result files should be n / 2 when n is number of files to multiply"
        <| fun ((size, n): int * int) ->
            if (n % 2 = 0) && (n > 0) && (size > 0)
            then
                let allFilesLength = helper size n (n / 2)
                Expect.equal allFilesLength (n + n / 2) ""

        testProperty "Number of result files should be n / 2 when n is number of files that user specified"
        <| fun ((size, n, fixedN): int * int * int) ->
            if (n % 2 = 0) && (n > 0) && (size > 0) && (fixedN > 0) && (fixedN < n / 2)
            then
                let allFilesLength = helper size n fixedN
                Expect.equal allFilesLength (n + fixedN) ""

        testProperty "Dimensions of results should have right sizes"
        <| fun ((size, n): int * int) ->
            if (n % 2 = 0) && (n > 0) && (size > 0)
            then
                let generatorConfig = generatorOptions(size, size, n, 0.75, tempPath, Int)
                generateSparseMatrix(generatorConfig)
                processFiles tempPath (n / 2) tempPathToSave
                let allGeneratedFiles = Directory.GetFiles(tempPath)
                for file in allGeneratedFiles do
                    File.Delete(file)
                let allResultFiles = Directory.GetFiles(tempPathToSave)
                for file in allResultFiles do
                    let matrix = read file
                    Expect.equal (matrix.GetLength(0)) size ""
                    Expect.equal (matrix.GetLength(1)) size ""
                    File.Delete(file)

  ]

module Tests
open System
open Expecto
open AlgebraicStructures
open MatrixLib
open SMatrixTypes
open SMatrixTransforms

let rand = Random()

let genRandomTree h =
    let rec go h =
        if h = 1
        then Leaf (rand.Next())
        else Node (go (h - 1), go (h - 1), go (h - 1), go (h - 1))
    go h
    
let generateSparseMatrix rows cols =
    let x = Array2D.init rows cols (fun _ _ -> rand.Next(0, 2))
    SparseMatrix (rows, cols,
                            [for i in 0..rows - 1 do
                                 for j in 0..cols - 1 do
                                     if x.[i, j] = 1 then (i, j, rand.Next(1, 100))])
    
let genArrayBySparseMatrix (matrix: SparseMatrix<int>) =
    let output = Array2D.zeroCreate matrix.numOfRows matrix.numOfCols
    for x in matrix.notEmptyCells do
        output.[first x, second x] <- third x
    output
    
let arrayToSparseMatrix (x: int[,]) =
    let y = Array.create (Array2D.length1 x * Array2D.length2 x) (0, 0, 0)
    let mutable z = 0
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            y.[z] <- i, j, x.[i,j]
            z <- z + 1
    SparseMatrix(Array2D.length1 x, Array2D.length2 x, y |> Array.filter (fun x -> (x |> third) <> 0) |> Array.toList)
    
let semiring = new Semiring<int> (new Monoid<int>((+), 0), (*))
let structure = Semiring semiring

[<Tests>]
let tests =
    testList "Tests for QuadTree" [
        testProperty "Sum" <| fun (dim: int) ->
            let dim = Math.Abs dim
            let sm1 = generateSparseMatrix dim dim
            let sm2 = generateSparseMatrix dim dim
            let m1 = genArrayBySparseMatrix sm1
            let m2 = genArrayBySparseMatrix sm2
            let sum1 = DenseOp.sum m1 m2 |> arrayToSparseMatrix |> toTree
            let sum2 = SparseOp.sum (sm1 |> toTree) (sm2 |> toTree) structure
            Expect.equal sum1 sum2 ""
                    
        testProperty "Multiply" <| fun (dim: int) ->
            let dim = Math.Abs dim
            let sm1 = generateSparseMatrix dim dim
            let sm2 = generateSparseMatrix dim dim
            let m1 = genArrayBySparseMatrix sm1
            let m2 = genArrayBySparseMatrix sm2
            let mul1 = DenseOp.multiply m1 m2 |> arrayToSparseMatrix |> toTree
            let mul2 = SparseOp.multiply (sm1 |> toTree) (sm2 |> toTree) structure
            Expect.equal mul1 mul2 ""
        
        testProperty "Parallel multiply" <| fun _ ->
            let x = genRandomTree 7
            let y = genRandomTree 7
            let res1 = SparseOp.multiply x y structure
            let res2 = SparseOp.parallelMultiply x y structure 3
            Expect.equal res1 res2 ""
                    
        testProperty "Tensor multiply" <| fun (dim: int) ->
            let dim = dim |> Math.Abs |> toPowerOf2
            let sm1 = generateSparseMatrix dim dim
            let sm2 = generateSparseMatrix dim dim
            let m1 = genArrayBySparseMatrix sm1
            let m2 = genArrayBySparseMatrix sm2
            let tmul1 = DenseOp.tensorMultiply m1 m2 |> arrayToSparseMatrix |> toTree
            let tmul2 = SparseOp.tensorMultiply (sm1 |> toTree) (sm2 |> toTree) structure
            Expect.equal tmul1 tmul2 ""]
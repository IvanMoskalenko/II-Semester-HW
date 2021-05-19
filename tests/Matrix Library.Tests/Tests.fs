module Tests
open System
open Expecto
open AlgebraicStructures
open MatrixLib
open SMatrixTypes
open SMatrixTransforms

let rand = Random()

let genIntTree h =
    let rec go h =
        if h = 1
        then Leaf (rand.Next())
        else Node (go (h - 1), go (h - 1), go (h - 1), go (h - 1))
    go h

let genSetTree h =
    let rec go h =
        if h = 1
        then
            let x = rand.Next(0, 10)
            if x = 0 then Leaf Set.empty
            else
                let arr = Array.zeroCreate x
                rand.NextBytes(arr)
                let set = Set.ofArray arr
                Leaf set        
        else Node (go (h - 1), go (h - 1), go (h - 1), go (h - 1))
    go h
    
let generateIntSparseMatrix rows cols =
    let x = Array2D.init rows cols (fun _ _ -> rand.Next(0, 2))
    SparseMatrix (rows, cols,
                            [for i in 0..rows - 1 do
                                 for j in 0..cols - 1 do
                                     if x.[i, j] = 1 then (i, j, rand.Next(1, 100))])

let generateSetSparseMatrix rows cols =
    let x = Array2D.init rows cols (fun _ _ -> rand.Next(0, 2))
    SparseMatrix (rows, cols,
                            [for i in 0..rows - 1 do
                                 for j in 0..cols - 1 do
                                     let z = rand.Next(0, 10)
                                     let arr = Array.zeroCreate z
                                     rand.NextBytes(arr)
                                     let set = Set.ofArray arr
                                     if x.[i, j] = 1 then (i, j, set)])
    
let genArrayBySparseMatrix (matrix: SparseMatrix<'t>) structure =
    let neutral = getNeutral structure
    let output = Array2D.create matrix.numOfRows matrix.numOfCols neutral
    for x in matrix.notEmptyCells do
        output.[first x, second x] <- third x
    output
       
let arrayToSparseMatrix x structure =
    let neutral = getNeutral structure
    let y = Array.create (Array2D.length1 x * Array2D.length2 x) (0, 0, neutral)
    let mutable z = 0
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            y.[z] <- i, j, x.[i,j]
            z <- z + 1
    SparseMatrix (Array2D.length1 x, Array2D.length2 x, y |> Array.filter (fun x -> (x |> third) <> neutral) |> Array.toList)
    
let intSemiring = new Semiring<int> (new Monoid<int>((+), 0), (*))
let setSemiring<'t when 't: comparison> =
    new Semiring<Set<'t>> (new Monoid<Set<'t>>((Set.union), Set.empty), (Set.intersect))
let intStructure = Semiring intSemiring
let setStructure<'t when 't: comparison> = Semiring setSemiring
               
[<Tests>]
let tests =
    testList "Tests for QuadTree" [
        testProperty "Sum for IntSemiring" <| fun (dim: int) ->
            let dim = Math.Abs dim
            let sm1 = generateIntSparseMatrix dim dim
            let sm2 = generateIntSparseMatrix dim dim
            let m1 = genArrayBySparseMatrix sm1 intStructure
            let m2 = genArrayBySparseMatrix sm2 intStructure
            let sum1 = arrayToSparseMatrix (DenseOp.sum m1 m2 intStructure) intStructure |> toTree
            let sum2 = SparseOp.sum (sm1 |> toTree) (sm2 |> toTree) intStructure
            Expect.equal sum1 sum2 ""
            
        testProperty "Sum for SetSemiring" <| fun (h: int) ->
            let h = Math.Abs h + 1
            if h < 10 then
                let t1 = genSetTree h
                let t2 = genSetTree h
                let m1 = toMatrix t1 (int (2.0 ** (float h - 1.0))) setStructure
                let m2 = toMatrix t2 (int (2.0 ** (float h - 1.0))) setStructure
                let sum1 = SparseOp.sum t1 t2 setStructure
                let sum2 = arrayToSparseMatrix (DenseOp.sum m1 m2 setStructure) setStructure |> toTree
                Expect.equal sum1 sum2 ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                    
        testProperty "Multiply for IntSemiring" <| fun (dim: int) ->
            let dim = Math.Abs dim
            let sm1 = generateIntSparseMatrix dim dim
            let sm2 = generateIntSparseMatrix dim dim
            let m1 = genArrayBySparseMatrix sm1 intStructure
            let m2 = genArrayBySparseMatrix sm2 intStructure
            let mul1 = arrayToSparseMatrix (DenseOp.multiply m1 m2 intStructure) intStructure |> toTree
            let mul2 = SparseOp.multiply (sm1 |> toTree) (sm2 |> toTree) intStructure
            Expect.equal mul1 mul2 ""
            
        testProperty "Multiply for SetSemiring" <| fun (h: int) ->
            let h = Math.Abs h + 1
            if h < 10 then
                let t1 = genSetTree h
                let t2 = genSetTree h
                let m1 = toMatrix t1 (int (2.0 ** (float h - 1.0))) setStructure
                let m2 = toMatrix t2 (int (2.0 ** (float h - 1.0))) setStructure
                let sum1 = SparseOp.multiply t1 t2 setStructure
                let sum2 = arrayToSparseMatrix (DenseOp.multiply m1 m2 setStructure) setStructure |> toTree
                Expect.equal sum1 sum2 "" 
        
        testProperty "Parallel multiply for IntSemiring" <| fun (h: int) ->
            let h = Math.Abs h + 1
            if h < 10 then
                let x = genIntTree h
                let y = genIntTree h
                let res1 = SparseOp.multiply x y intStructure
                let res2 = SparseOp.parallelMultiply x y intStructure 2
                Expect.equal res1 res2 ""
        
        testProperty "Parallel multiply for SetSemiring" <| fun (h: int) ->
            let h = Math.Abs h + 1
            if h < 10 then
                let x = genSetTree h
                let y = genSetTree h
                let res1 = SparseOp.multiply x y setStructure
                let res2 = SparseOp.parallelMultiply x y setStructure 2
                Expect.equal res1 res2 ""]
module Mailbox.MultiplyFunctions
open MatrixLib.AlgebraicStructures
open MatrixLib.SparseMatrixQT
open MatrixLib

let intStructure =
    { GetGenericZero = fun () -> 0
      Addition = (+)
      Multiplication = (*) }

let quadTreeMultiply a b =
    SparseMath.multiply intStructure a b


// TODO: parallel with number of threads
let quadTreeParallelMultiply a b =
    SparseMath.multiply intStructure a b

let arrMultiply a b = Dense.multiply a b intStructure

// TODO: number of threads
let arrMultiplyParallel a b = Dense.multiplyParallel 2 a b intStructure


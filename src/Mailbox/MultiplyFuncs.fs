module Mailbox.MultiplyFunctions
open MatrixLib
open AlgebraicStructures
open MatrixAlgebra
open Array2dMatrices

let intStructure =
    { GetZero = fun () -> 0
      Eq = (=)
      AddOp = (+)
      MulOp= (*) }

let quadTreeMultiply a b =
    MatrixAlgebra.multiply intStructure a b

let quadTreeParallelMultiply a b =
    MatrixAlgebra.multiplyParallel intStructure a b 2

let arrMultiply a b =
    MatrixArray2D.multiply a b intStructure

let arrMultiplyParallel a b =
   MatrixArray2D.multiplyParallel a b intStructure


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
    async { return MatrixAlgebra.multiply intStructure a b }

let quadTreeParallelMultiply a b =
    async { return MatrixAlgebra.multiplyParallel intStructure a b 2 }

let arrMultiply a b =
    async { return MatrixArray2D.multiply a b intStructure }

let arrMultiplyParallel a b =
   async { return MatrixArray2D.multiplyParallel a b intStructure }


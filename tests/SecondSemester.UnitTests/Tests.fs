module PointRegionQuadtree.UnitTests.MainTests
open Expecto                        // Test Framework
open MatrixLib
open MatrixLib.MatrixAlgebra
open MatrixLib.Array2dMatrices
open MatrixLib.AlgebraicStructures  // Semirings
open MatrixLib.SparseMatrixQT       // Sparse matrix functions

let tests =
    testSequenced <| testList "Math operations on quadtrees" [
        let semiring =
                    { GetZero = fun () -> 0
                      Eq = (=)
                      AddOp = (+)
                      MulOp = (*) }

        let helperMult (matrixA: int[,]) (matrixB: int[,]) =
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = multiply matrixA matrixB semiring
            let resSparse = MatrixAlgebra.multiply semiring sparseA sparseB
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected

        let helperSum (matrixA: int[,]) (matrixB: int[,]) =
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = add matrixA matrixB semiring
            let resSparse = MatrixAlgebra.sum semiring sparseA sparseB
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected

        let matrixA = array2D [|
                    [|5; 0; 4; 6|]
                    [|0; 28; 7; 0|]
                    [|0; 5; 1; 0|]
                    [|0; 0; 1; 7|]
                |]
        let matrixB = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 8; 0; 0|]
                    [|0; 5; 2; 0|]
                    [|8; 0; 1; 7|]
                |]

        let matrixC = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 0; 7; 0|]
                    [|0; 0; 5; 0|]
                    [|0; 0; 1; 7|]
                |]
        let matrixD = array2D [|
                    [|4; 0; 0; 6|]
                    [|0; 8; 5; 0|]
                    [|0; 0; 2; 0|]
                    [|8; 0; 1; 8|]
                |]
        let matrixE = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
        let matrixF = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
        let matrixG = array2D [|
                    [|7; 1; 8; 9; 2; 5; 9; 18|]
                    [|6; 0; 8; 9; 2; 5; 4; 1|]
                    [|7; 0; 5; 9; 2; 4; 92; 12|]
                    [|7; 0; 8; 9; 2; 5; 9; 1|]
                    [|0; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 59; 19; 15|]
                    [|0; 0; 0; 0; 2; 5; 9; 19|]
                    [|0; 0; 0; 0; 2; 5; 9; 12|]
                |]
        let matrixH = array2D [|
                    [|0; 0; 0; 0; 2; 5; 9; 18|]
                    [|0; 0; 0; 9; 2; 5; 4; 1|]
                    [|0; 0; 0; 0; 2; 4; 7; 16|]
                    [|0; 0; 8; 0; 67; 5; 12; 1|]
                    [|6; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 4; 19; 11|]
                    [|0; 9; 0; 0; 6; 4; 9; 9|]
                    [|0; 0; 7; 0; 2; 4; 9; 1|]
                |]

        testCase "Sum test 1" <| fun () ->
            let actual, expected = helperSum matrixA matrixB
            Expect.equal actual expected ""
        testCase "Sum test 2" <| fun () ->
            let actual, expected = helperSum matrixC matrixD
            Expect.equal actual expected ""
        testCase "Sum test 3" <| fun () ->
            let actual, expected = helperSum matrixE matrixF
            Expect.equal actual expected ""
        testCase "Sum test 4" <| fun () ->
            let actual, expected = helperSum matrixG matrixH
            Expect.equal actual expected ""

        testCase "Multiply test 1" <| fun () ->
            let matrixA = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
            let matrixB = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = multiply matrixA matrixB semiring
            let resSparse = MatrixAlgebra.multiply semiring sparseA sparseB

            let actual = SparseMatrixQT.toArray2D 0 resSparse
            Expect.equal actual expected ""
        testCase "Multiply test 2" <| fun () ->
            let actual, expected = helperMult matrixC matrixD
            Expect.equal actual expected ""
        testCase "Multiply test 3" <| fun () ->
            let actual, expected = helperMult matrixE matrixF
            Expect.equal actual expected ""
        testCase "Multiply test 4" <| fun () ->
            let actual, expected = helperMult matrixG matrixH
            Expect.equal actual expected ""

        testCase "Init test 1" <| fun () ->
            let matrix = array2D [|
                    [|5; 0; 4; 6|]
                    [|0; 28; 7; 0|]
                    [|0; 5; 1; 0|]
                    [|0; 0; 1; 7|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 2" <| fun () ->
            let matrix = array2D [|
                    [|7; 1;|]
                    [|0; 8;|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 3" <| fun () ->
            let matrix = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 0; 7; 0|]
                    [|0; 0; 5; 0|]
                    [|0; 0; 1; 7|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 4" <| fun () ->
            let matrix = array2D [|
                    [|4; 0; 0; 6|]
                    [|0; 8; 5; 0|]
                    [|0; 0; 2; 0|]
                    [|8; 0; 1; 8|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 5" <| fun () ->
            let matrix = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 6" <| fun () ->
            let matrix = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 7" <| fun () ->
            let matrix = array2D [|
                    [|7; 1; 8; 9; 2; 5; 9; 18|]
                    [|6; 0; 8; 9; 2; 5; 4; 1|]
                    [|7; 0; 5; 9; 2; 4; 92; 12|]
                    [|7; 0; 8; 9; 2; 5; 9; 1|]
                    [|0; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 59; 19; 15|]
                    [|0; 0; 0; 0; 2; 5; 9; 19|]
                    [|0; 0; 0; 0; 2; 5; 9; 12|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

        testCase "Init test 8" <| fun () ->
            let matrix = array2D [|
                    [|0; 0; 0; 0; 2; 5; 9; 18|]
                    [|0; 0; 0; 9; 2; 5; 4; 1|]
                    [|0; 0; 0; 0; 2; 4; 7; 16|]
                    [|0; 0; 8; 0; 67; 5; 12; 1|]
                    [|6; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 4; 19; 11|]
                    [|0; 9; 0; 0; 6; 4; 9; 9|]
                    [|0; 0; 7; 0; 2; 4; 9; 1|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""

    ]

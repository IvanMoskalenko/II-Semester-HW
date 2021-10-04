module Worker
open Mailbox
open Multiplier
open MultiplyFunctions
open Helpers
open MatrixLib.SparseMatrixQT

let arrToQT arr =
    (SparseMatrixQT (arr, (fun () -> 0), (=)))

let deconverter qt =
    SparseMatrixQT.toArray2D 0 qt

let processFiles path amount =
    let balancer = Balancer.balancer
                       (multiplier quadTreeMultiply arrToQT deconverter)
                       (multiplier quadTreeParallelMultiply arrToQT deconverter)
                       (multiplier arrMultiply id id)
                       (multiplier arrMultiplyParallel id id)

    let mtxLoader = Loader.loader path balancer amount

    mtxLoader.PostAndReply Go


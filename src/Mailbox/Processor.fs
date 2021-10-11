module Worker
open Mailbox
open Multiplier
open MultiplyFunctions
open Helpers
open MatrixLib.SparseMatrixQT

let arrToQt arr =
    async { return (SparseMatrixQT (arr, (fun () -> 0), (=))) }
let QtToArr qt =
    async { return SparseMatrixQT.toArray2D 0 qt }
let id x = async { return x }
let processFiles path amount saveDir =
    let balancer = Balancer.balancer
                       (multiplier quadTreeMultiply arrToQt QtToArr saveDir)
                       (multiplier quadTreeParallelMultiply arrToQt QtToArr saveDir)
                       (multiplier arrMultiply id id saveDir)
                       (multiplier arrMultiplyParallel id id saveDir)

    let mtxLoader = Loader.loader path balancer amount
    mtxLoader.PostAndReply Go


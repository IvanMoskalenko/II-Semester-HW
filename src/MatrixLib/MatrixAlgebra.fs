module MatrixLib.MatrixAlgebra

open System.Collections.Generic
open PointRegionQuadtree.QtTypes.MatrixCell
open PointRegionQuadtree.MutableQT

open MatrixLib.AlgebraicStructures
open MatrixLib.SparseMatrixQT

module MatrixAlgebra =
    let sum sr (mtx1: SparseMatrixQT<_>) (mtx2: SparseMatrixQT<_>) =
        let res =
            MutableQT.sum (sr.GetZero, sr.Eq, sr.AddOp)
                mtx1.quadtree mtx2.quadtree

        SparseMatrixQT(res, sr.GetZero)

    let multiplyOnInsert sr (mtxA: SparseMatrixQT<_>) (mtxB: SparseMatrixQT<_>) =
        // check square matrix size
        if mtxA.size <> mtxB.size then
            failwith $"Incorrect size of input matrices: {mtxA.size}x{mtxA.size}, {mtxB.size}x{mtxB.size}"

        let mutable res =
            SparseMatrixQT(mtxA.size, sr.GetZero)

        for x in 0 .. mtxA.size - 1 do
            for y in 0 .. mtxB.size - 1 do
                let mutable acc = sr.GetZero()
                for z in 0 .. mtxA.size - 1 do
                    acc <- sr.AddOp acc (sr.MulOp mtxA.[x, z] mtxB.[z, y])
                res.[x, y] <- acc
        res

    let tensorMultiply' semiring (mtxA: SparseMatrixQT<_>) (mtxB: SparseMatrixQT<_>) =
        let newLeafCoord (X, Y) delta pointB =
            let Xb, Yb = pointB
            ((X * delta) + Xb, (Y * delta) + Yb)

        let X1, Y1 =
            mtxA.quadtree.Region.SizeX,
            mtxA.quadtree.Region.SizeY

        let X2, Y2 =
            mtxB.quadtree.Region.SizeX,
            mtxB.quadtree.Region.SizeY

        let x, y = float (X1 * X2) / 2.0, float (Y1 * Y2) / 2.0
        let SizeX = X1 * X2
        let _SizeY = Y1 * Y2
        let resTree = emptyTree (MatrixCell(x, y, SizeX))

        let rec _go (treeA: Quadtree<_,_>) (treeB: Quadtree<_,_>) =
            match treeA.Content, treeB.Content with
            | Leaf (pointA, a), Leaf (pointB, b) ->
                let fixedCoord =
                    newLeafCoord pointA mtxB.size pointB
                let res = semiring.MulOp a b
                if res <> semiring.GetZero()
                then MutableQT.insert fixedCoord res resTree |> ignore
                else ()
            | Leaf _, Nodes (y1, y2, y3, y4) ->
                _go treeA y1
                _go treeA y2
                _go treeA y3
                _go treeA y4
            | Nodes (x1, x2, x3, x4), _ ->
                _go x1 treeB
                _go x2 treeB
                _go x3 treeB
                _go x4 treeB
            | Empty, _ -> ()
            | _, Empty -> ()

        _go mtxA.quadtree mtxB.quadtree
        SparseMatrixQT(resTree, semiring.GetZero)

    let tensorMultiply semiring (mtxA: SparseMatrixQT<HashSet<_>>) (mtxB: SparseMatrixQT<HashSet<_>>) =
        let multiplicationSets (s1:HashSet<_>) (s2: HashSet<_>) =
            let res = HashSet<_>(s1) in res.IntersectWith s2
            res

        let X1, Y1 =
            mtxA.quadtree.Region.SizeX,
            mtxA.quadtree.Region.SizeY

        let X2, Y2 =
            mtxB.quadtree.Region.SizeX,
            mtxB.quadtree.Region.SizeY

        let rows = int (X1 * X2)
        let columns = int (Y1 * Y2)
        let mutable res = SparseMatrixQT(emptyTree (MatrixCell(rows)), semiring.GetZero)
        for i in 0..rows - 1 do
            for j in 0..columns - 1 do
                let first = mtxA.[i / mtxB.size, j / mtxB.size ]
                let second = mtxB.[i % mtxB.size, j % mtxB.size]
                let kek = multiplicationSets first second
                res.[i, j] <- kek
        res

    let private cellsCouldBeMult (A: MatrixCell) (B: MatrixCell) =
        // check regions centers
        if A.center <> B.center
        then failwith "Error: Multiplication of quadtree matrices with different centers of regions"

        // now let's check sizes (multiplication cond.)
        if A.size <> B.size
        then failwith "Error: Multiplication of square matrices with different sizes"

    let private subdivideLeaf leaf parentTree =
        match MutableQT.balanceLeaf leaf parentTree with
        | Error msg -> failwith $"Failed to subdivide; {msg}"
        | Ok balanced -> balanced

    let multiply (sr: Semiring<'a>) (mtx1: SparseMatrixQT<_>) (mtx2: SparseMatrixQT<_>) =
        let monoid = sr.GetZero, sr.Eq, sr.AddOp
        // recursive multiply of square region quadtree
        let rec loop (curr: CellParams) qt1 qt2 =
            match qt1.Content, qt2.Content with
            | Empty, _ -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | _, Empty -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | Leaf (p1, v1), Nodes (nw2, ne2, sw2, se2) ->
                let nw1, ne1, sw1, se1 = subdivideLeaf (p1, v1) qt1
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw nw1 nw2) (loop _nw ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne nw1 ne2) (loop _ne ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw sw1 nw2) (loop _sw se1 sw2)
                let SE = MutableQT.sum monoid (loop _se sw1 ne2) (loop _se se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Nodes (nw1, ne1, sw1, se1), Leaf (p2, v2) ->
                let nw2, ne2, sw2, se2 = subdivideLeaf (p2, v2) qt2
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw nw1 nw2) (loop _nw ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne nw1 ne2) (loop _ne ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw sw1 nw2) (loop _sw se1 sw2)
                let SE = MutableQT.sum monoid (loop _se sw1 ne2) (loop _se se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Leaf (point1, A), Leaf (point2, B) when curr.Size = 1 ->
                let i1, _j1 = point1
                let _i2, j2 = point2
                Leaf ((i1, j2), (sr.MulOp A B)) |> makeTree (MatrixCell curr)

            | Leaf (point1, A), Leaf (point2, B) ->
                let nw1, ne1, sw1, se1 = subdivideLeaf (point1, A) qt1
                let nw2, ne2, sw2, se2 = subdivideLeaf (point2, B) qt2
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw nw1 nw2) (loop _nw ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne nw1 ne2) (loop _ne ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw sw1 nw2) (loop _sw se1 sw2)
                let SE = MutableQT.sum monoid (loop _se sw1 ne2) (loop _se se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw nw1 nw2) (loop _nw ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne nw1 ne2) (loop _ne ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw sw1 nw2) (loop _sw se1 sw2)
                let SE = MutableQT.sum monoid (loop _se sw1 ne2) (loop _se se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

        let reg1, reg2 = mtx1.quadtree.Region, mtx2.quadtree.Region
        match (reg1 :? MatrixCell) && (reg2 :? MatrixCell) with
        | false ->
            failwith
                <| "Only 'SquareMatrixCell' quadtree is allowed;"
                    + $" got regions: {reg1.GetType()}, {reg2.GetType()}"
        | true ->
            // at this point we already checked type of region
            let cellA = reg1 :?> MatrixCell
            let cellB = reg2 :?> MatrixCell

            // check cell regions
            cellsCouldBeMult cellA cellB
            // at this point
            // we are allowed to make an assumption
            // that sizes and centers of matrix cells is equal

            let resultTree =
                loop (CellParams(cellA.center, cellA.size))
                    mtx1.quadtree
                    mtx2.quadtree

            SparseMatrixQT (resultTree, sr.GetZero)

    let multiplyParallel (sr: Semiring<'a>) (mtx1: SparseMatrixQT<_>) (mtx2: SparseMatrixQT<_>) depth =
        let monoid = sr.GetZero, sr.Eq, sr.AddOp
        // recursive multiply of square region quadtree
        let rec loop (curr: CellParams) c qt1 qt2 =
            match qt1.Content, qt2.Content with
            | Empty, _ -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | _, Empty -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | Leaf (p1, v1), Nodes (nw2, ne2, sw2, se2) ->
                let nw1, ne1, sw1, se1 = subdivideLeaf (p1, v1) qt1
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw c nw1 nw2) (loop _nw c ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne c nw1 ne2) (loop _ne c ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw c sw1 nw2) (loop _sw c se1 sw2)
                let SE = MutableQT.sum monoid (loop _se c sw1 ne2) (loop _se c se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Nodes (nw1, ne1, sw1, se1), Leaf (p2, v2) ->
                let nw2, ne2, sw2, se2 = subdivideLeaf (p2, v2) qt2
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw c nw1 nw2) (loop _nw c ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne c nw1 ne2) (loop _ne c ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw c sw1 nw2) (loop _sw c se1 sw2)
                let SE = MutableQT.sum monoid (loop _se c sw1 ne2) (loop _se c se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Leaf (point1, A), Leaf (point2, B) when curr.Size = 1 ->
                // todo: i have no idea if this is ALWAYS correct
                let i1, _j1 = point1
                let _i2, j2 = point2
                Leaf ((i1, j2), (sr.MulOp A B)) |> makeTree (MatrixCell curr)

            | Leaf (point1, A), Leaf (point2, B) ->
                let nw1, ne1, sw1, se1 = subdivideLeaf (point1, A) qt1
                let nw2, ne2, sw2, se2 = subdivideLeaf (point2, B) qt2
                let _nw, _ne, _sw, _se = getSubsParams curr
                let NW = MutableQT.sum monoid (loop _nw c nw1 nw2) (loop _nw c ne1 sw2)
                let NE = MutableQT.sum monoid (loop _ne c nw1 ne2) (loop _ne c ne1 se2)
                let SW = MutableQT.sum monoid (loop _sw c sw1 nw2) (loop _sw c se1 sw2)
                let SE = MutableQT.sum monoid (loop _se c sw1 ne2) (loop _se c se1 se2)
                Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

            | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
                let _nw, _ne, _sw, _se = getSubsParams curr
                if c < depth
                then
                    let NW = async {return MutableQT.sum monoid (loop _nw (c + 1) nw1 nw2) (loop _nw (c + 1) ne1 sw2)}
                    let NE = async {return MutableQT.sum monoid (loop _ne (c + 1) nw1 ne2) (loop _ne (c + 1) ne1 se2)}
                    let SW = async {return MutableQT.sum monoid (loop _sw (c + 1) sw1 nw2) (loop _sw (c + 1) se1 sw2)}
                    let SE = async {return MutableQT.sum monoid (loop _se (c + 1) sw1 ne2) (loop _se (c + 1) se1 se2)}
                    let res = [NW; NE; SW; SE] |> Async.Parallel |> Async.RunSynchronously
                    Nodes (res.[0], res.[1], res.[2], res.[3]) |> makeTree (MatrixCell curr)
                else
                    let NW = MutableQT.sum monoid (loop _nw c nw1 nw2) (loop _nw c ne1 sw2)
                    let NE = MutableQT.sum monoid (loop _ne c nw1 ne2) (loop _ne c ne1 se2)
                    let SW = MutableQT.sum monoid (loop _sw c sw1 nw2) (loop _sw c se1 sw2)
                    let SE = MutableQT.sum monoid (loop _se c sw1 ne2) (loop _se c se1 se2)
                    Nodes (NW, NE, SW, SE) |> makeTree (MatrixCell curr)

        let reg1, reg2 = mtx1.quadtree.Region, mtx2.quadtree.Region
        match (reg1 :? MatrixCell) && (reg2 :? MatrixCell) with
        | false ->
            failwith
                <| "Only 'SquareMatrixCell' quadtree is allowed;"
                    + $" got regions: {reg1.GetType()}, {reg2.GetType()}"
        | true ->
            // at this point we already checked type of region
            let cellA = reg1 :?> MatrixCell
            let cellB = reg2 :?> MatrixCell

            // check cell regions
            cellsCouldBeMult cellA cellB
            // at this point
            // we are allowed to make an assumption
            // that sizes and centers of matrix cells is equal

            let resultTree =
                loop (CellParams(cellA.center, cellA.size)) 0
                    mtx1.quadtree
                    mtx2.quadtree

            SparseMatrixQT (resultTree, sr.GetZero)

    let closure sr condition (mtx: SparseMatrixQT<_>) =
        let count cond (mtx': SparseMatrixQT<_>) =
            let mutable count = 0

            mtx'
            |> SparseMatrixQT.iter
                (fun elem ->
                    if cond elem then
                        count <- count + 1)

            count

        let mutable result = mtx
        let mutable _continue = true

        while _continue do
            let prev = count condition result
            let multiplied = multiply sr result result

            let added = (sum sr result multiplied)
            result <- added
            let current =
                count condition added

            if prev = current then
                _continue <- false

        result



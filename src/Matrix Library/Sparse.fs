namespace MatrixLib
module SMatrixTypes =
    type QuadTree<'t> =
        | None
        | Leaf of 't
        | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>
           
    type SparseMatrix<'t> =
        val numOfRows: int
        val numOfCols : int
        val notEmptyCells: list<int * int * 't>
        new (x, y, lst) = {numOfRows = x; numOfCols = y; notEmptyCells = lst}   
        
module SparseOp =
    open AlgebraicStructures
    open SMatrixTypes
            
    let sum x y structure =
        let operation, neutral = getOperationAndNeutral structure false
        let rec _go x y =
            match x, y with
            | Leaf a, Leaf b ->
                let res = operation a b
                if res = neutral then None else Leaf res
            | None, k -> k
            | k, None -> k
            | Node (x1, x2, x3, x4), Node (y1, y2, y3, y4) ->
                let NW = _go x1 y1
                let NE = _go x2 y2
                let SW = _go x3 y3
                let SE = _go x4 y4
                if NW = None && NE = None && SW = None && SE = None then None
                else Node (NW, NE, SW, SE)
            | _, _ -> failwith "It's impossible to sum this"
        _go x y
        
    let multiply x y structure =
        let operation, neutral = getOperationAndNeutral structure true
        let rec _go x y =
            match x, y with
            | Leaf a, Leaf b ->
                let res = operation a b
                if res = neutral then None else Leaf res
            | None, _ -> None
            | _, None -> None
            | Node (x1, x2, x3, x4), Node (y1, y2, y3, y4) ->
                let NW = sum (_go x1 y1) (_go x2 y3) structure
                let NE = sum (_go x1 y2) (_go x2 y4) structure
                let SW = sum (_go x3 y1) (_go x4 y3) structure
                let SE = sum (_go x3 y2) (_go x4 y4) structure
                if NW = None && NE = None && SW = None && SE = None then None
                else Node (NW, NE, SW, SE)
            | _, _ -> failwith "It's impossible to multiply this"
        _go x y
        
    let parallelMultiply x y structure deepness =
        let operation, neutral = getOperationAndNeutral structure true
        let rec _go x y c =
            match x, y with
            | Leaf a, Leaf b ->
                let res = operation a b
                if res = neutral then None else Leaf res
            | None, _ -> None
            | _, None -> None
            | Node (x1, x2, x3, x4), Node (y1, y2, y3, y4) ->
                if c < deepness
                then
                    let NW = async {return sum (_go x1 y1 (c + 1)) (_go x2 y3 (c + 1)) structure}
                    let NE = async {return sum (_go x1 y2 (c + 1)) (_go x2 y4 (c + 1)) structure}
                    let SW = async {return sum (_go x3 y1 (c + 1)) (_go x4 y3 (c + 1)) structure}
                    let SE = async {return sum (_go x3 y2 (c + 1)) (_go x4 y4 (c + 1)) structure}
                    let res = [NW; NE; SW; SE] |> Async.Parallel |> Async.RunSynchronously
                    if res.[0] = None && res.[1] = None && res.[2] = None && res.[3] = None then None
                    else Node (res.[0], res.[1], res.[2], res.[3])
                else
                    let NW = sum (_go x1 y1 c) (_go x2 y3 c) structure
                    let NE = sum (_go x1 y2 c) (_go x2 y4 c) structure
                    let SW = sum (_go x3 y1 c) (_go x4 y3 c) structure
                    let SE = sum (_go x3 y2 c) (_go x4 y4 c) structure
                    if NW = None && NE = None && SW = None && SE = None then None
                    else Node (NW, NE, SW, SE)
            | _, _ -> failwith "It's impossible to multiply this"
        _go x y 1
        
    let scalarMultiply x scalar structure =
        let operation, neutral = getOperationAndNeutral structure true
        let rec _go x =
            match x with
            | Leaf t -> Leaf (operation scalar t)
            | None -> None
            | Node (NW, NE, SW, SE) -> Node (_go NW, _go NE, _go SW, _go SE)
        if scalar = neutral then None else _go x
        
    let tensorMultiply x y structure =
        if x = None || y = None then None
        else
            let rec _go x =
                match x with
                | Leaf t -> scalarMultiply y t structure
                | None -> None
                | Node (NW, NE, SW, SE) -> Node (_go NW, _go NE, _go SW, _go SE)
            _go x
    
module SMatrixTransforms =
    open SMatrixTypes
    
    let toMatrix quadTree size =
        
        let output = Array2D.create size size 0    
        let rec _go quadTree (x, y) size =
            let hSize = size / 2
            let qSize = size / 4
            match quadTree, size with
            | Leaf t, _ -> output.[x, y] <- t
            | None, _ -> output.[0, 0] <- output.[0, 0]
            | Node (q1, q2, q3, q4), 2 ->
                let nwQP = x - hSize, y - hSize
                let neQP = x - hSize, y
                let swQP = x, y - hSize
                let seQP = x, y
                _go q1 nwQP hSize; _go q2 neQP hSize; _go q3 swQP hSize; _go q4 seQP hSize
            | Node (q1, q2, q3, q4), _ ->
                let nwQP = x - qSize, y - qSize
                let neQP = x - qSize, y + qSize
                let swQP = x + qSize, y - qSize
                let seQP = x + qSize, y + qSize
                _go q1 nwQP hSize; _go q2 neQP hSize; _go q3 swQP hSize; _go q4 seQP hSize
        _go quadTree (size / 2, size / 2) size
        
        let toSparse matrix =
            let mutable k = 0
            for i in 0 .. Array2D.length1 matrix - 1 do
                for j in 0 .. Array2D.length2 matrix - 1 do
                    if matrix.[i,j] <> 0
                    then k <- k + 1
            let unrealIndex = Array2D.length1 matrix + 1
            let output = Array.create k (unrealIndex, unrealIndex, 0)
            k <- 0
            for i in 0 .. Array2D.length1 matrix - 1 do
                for j in 0 .. Array2D.length2 matrix - 1 do
                    if matrix.[i,j] <> 0
                    then
                        output.[k] <- i, j, matrix.[i,j]
                        k <- k + 1
            SparseMatrix (unrealIndex - 1, unrealIndex - 1, List.ofArray output)        
        toSparse output
        
    let first (x, _, _) = x
    let second (_, x, _) = x
    let third (_, _, x) = x
    let toPowerOf2 x =
            let rec go r = if pown 2 r < x then go (r + 1) else r
            pown 2 (go 0)
            
    let toTree matrix =      
        
        let toPowOf2SizedMatrix (matrix: SparseMatrix<'t>) =
            let max = max matrix.numOfRows matrix.numOfCols
            SparseMatrix (toPowerOf2 max, toPowerOf2 max, matrix.notEmptyCells)            
                
        let divTo4 (matrix: SparseMatrix<'t>) =
            let l1 = List.filter (fun (i, j, _) -> (i + 1 <= (matrix.numOfRows / 2) && j + 1 <= (matrix.numOfCols / 2))) matrix.notEmptyCells
            let q1 = new SparseMatrix<'t>(matrix.numOfRows / 2, matrix.numOfCols /2, l1)
            let l2 = List.filter (fun (i, j, _) -> (i + 1 <= (matrix.numOfRows / 2) && j + 1 > (matrix.numOfCols / 2))) matrix.notEmptyCells
            let l2n = List.map (fun (i, j, k) -> i, j - matrix.numOfCols/2, k) l2
            let q2 = new SparseMatrix<'t>(matrix.numOfRows / 2, matrix.numOfCols /2, l2n)
            let l3 = List.filter (fun (i, j, _) -> (i + 1 > (matrix.numOfRows/2) && j + 1 <= (matrix.numOfCols / 2))) matrix.notEmptyCells
            let l3n = List.map (fun (i, j, k) -> i - matrix.numOfRows/2, j, k) l3
            let q3 = new SparseMatrix<'t>(matrix.numOfRows / 2, matrix.numOfCols /2, l3n)
            let l4 = List.filter (fun (i, j, _) -> (i + 1 > (matrix.numOfRows / 2) && j + 1 > (matrix.numOfCols / 2))) matrix.notEmptyCells
            let l4n = List.map (fun (i, j, k) -> i - matrix.numOfRows / 2, j - matrix.numOfCols / 2, k) l4
            let q4 = new SparseMatrix<'t>(matrix.numOfRows / 2, matrix.numOfCols / 2, l4n)
            (q1, q2, q3, q4)
                
        let rec f (matrix: SparseMatrix<'t>) =
            if List.isEmpty matrix.notEmptyCells then QuadTree.None
            elif matrix.numOfRows = 1 && matrix.numOfCols = 1
            then QuadTree.Leaf <| (matrix.notEmptyCells.Head |> third)
            else
                let q1, q2, q3, q4 = divTo4 matrix
                QuadTree.Node (f q1, f q2, f q3, f q4)
        
        f (toPowOf2SizedMatrix matrix)




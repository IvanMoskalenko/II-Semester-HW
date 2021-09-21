module AlgebraicStructures

type Monoid<'t> =
    val sum: 't -> 't -> 't
    val neutral: 't
    new (x, y) = {sum = x; neutral = y}

type Semiring<'t> =
    val monoid: Monoid<'t>
    val multiply: 't -> 't -> 't
    new (x, y) = {monoid = x; multiply = y}

type AlgebraicStructure<'t> =
    | Monoid of Monoid<'t>
    | Semiring of Semiring<'t>

let getNeutral structure =
    let neutral =
         match structure with
         | Monoid x -> x.neutral
         | Semiring x -> x.monoid.neutral
    neutral

let getSumOperation structure =
    let sumOperation =
         match structure with
         | Monoid x -> x.sum
         | Semiring x -> x.monoid.sum
    sumOperation
    
let getMultiplyOperation structure =
    let multiplyOperation =
         match structure with
         | Monoid _ -> failwith "Can't multiply in monoid"
         | Semiring x -> x.multiply
    multiplyOperation
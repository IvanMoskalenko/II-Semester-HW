module AlgebraicStructures

type Monoid<'t> =
    val sum: 't -> 't -> 't
    val neutral: 't
    new (x, y) = {sum = x; neutral = y}

type Semiring<'t>  =
    val monoid: Monoid<'t>
    val multiply: 't -> 't -> 't
    new (x, y) = {monoid = x; multiply = y}

type AlgebraicStructure<'t> =
    | Monoid of Monoid<'t>
    | Semiring of Semiring<'t>

let getOperationAndNeutral structure isMultiply =
    let operation, neutral =
         match structure with
         | Monoid x -> x.sum, x.neutral
         | Semiring x ->
             if isMultiply then x.multiply, x.monoid.neutral
             else x.monoid.sum, x.monoid.neutral
    operation, neutral


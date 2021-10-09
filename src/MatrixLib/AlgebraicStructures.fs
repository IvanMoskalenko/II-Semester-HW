module MatrixLib.AlgebraicStructures

// IsReadOnly attribute for structs
open System.Runtime.CompilerServices

// -> GetZero   - is a generator of zero elem. in semiring of type 'a
// -> Eq        - is a custom equality operator defined for type 'a
// -> AddOp     - is an addition ('+' operator) in semiring of type 'a
// -> MulOp     - is a multiplication ('*' operator) in semiring of type 'a

(* Note: this is not an actual algebraic semiring, because it does not fully implement monoids:
    - type does not declare explicit additive identity;
    - type does not declare explicit multiplicative identity;
    In some cases, GenericZero is assumed to be additive identity. *)

/// Semiring type with generic zero value (of specified type)
/// and two binary operators: addition, multiplication
[<IsReadOnly; Struct>]
type Semiring<'a> =
    { GetZero: unit -> 'a
      Eq: 'a -> 'a -> bool
      AddOp: 'a -> 'a -> 'a
      MulOp: 'a -> 'a -> 'a }

/// Monoid type with associative binary operators and an identity element.
type Monoid<'a> =
    { GetIdentityElement: unit -> 'a
      AssociativeBinOp: 'a -> 'a -> 'a }

let unwrap sr = sr.GetZero, sr.Eq, sr.AddOp, sr.MulOp

let additiveMonoidOf sr =  sr.GetZero, sr.Eq, sr.AddOp

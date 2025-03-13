(*** Exercise: refactor arith ***)

(* OfInt module type, analogy to one interface *)
module type OfInt = sig
    type t
    val of_int: int -> t
end

module type RingWithoutOfInt = sig
    type t

    val zero : t

    val one : t

    val ( + ) : t -> t -> t

    val ( ~- ) : t -> t

    val ( * ) : t -> t -> t

    val to_string : t -> string
end

(*Ring module type which implements OfInt *)
module type Ring = sig
    include OfInt

    include RingWithoutOfInt with type t := t
end

module ImplementOfInt (M: RingWithoutOfInt) = (struct
    include M 

    let rec of_int x = 
        if x > 0 then ( + ) zero (of_int (Stdlib.(-) x 1))
        else if x < 0 then ( + ) zero (of_int (Stdlib.(+) x 1))
        else zero
end: Ring with type t = M.t)

module IntRingWithoutOfInt: RingWithoutOfInt with type t = int = struct
    type t = int

    let zero = 0

    let one = 1

    let ( + ) = ( + )

    let ( ~- ) = ( ~- )

    let ( * ) = ( * )

    let to_string = string_of_int
end

module FloatRingWithoutOfInt: RingWithoutOfInt with type t = float = struct
    type t = float

    let zero = 0.

    let one = 1.

    let ( + ) = ( +. )

    let ( ~- ) = ( ~-. )

    let ( * ) = ( *. )

    let to_string = string_of_float
end

module IntRing: Ring with type t = int = ImplementOfInt(IntRingWithoutOfInt)

module FloatRing: Ring with type t = float = ImplementOfInt(FloatRingWithoutOfInt)

module type Field = sig
    include Ring
    
    val ( / ): t -> t -> t
end

module IntField: Field = struct
    include IntRing

    let ( / ) = ( / )
end

module FloatField: Field = struct
    include FloatRing

    let ( / ) = ( /. )
end

module RationalizeField (M: Field) = struct
    include M

    type t = M.t * M.t

    let zero = (M.zero, M.one)

    let one = (M.one, M.one)

    let ( + ) (n1, d1) (n2, d2) = (M.(+) (M.( * ) n1 d2) (M.( * ) n2 d1), M.( * ) d1 d2)
    
    let ( ~- ) (n1, d1) = (M.(~-) n1, d1)

    let ( * ) (n1, d1) (n2, d2) = (M.( * ) n1 n2, M.( * ) d1 d2)

    let ( / ) (n1, d1) (n2, d2) = (M.( * ) n1 d2, M.( * ) d1 n2)

    let to_string (n, d) = M.to_string n ^ "/" ^ M.to_string d
end

module IntRationalFieldWithoutOfInt = RationalizeField(IntField)

module IntRational: Field= struct
    include IntRationalFieldWithoutOfInt
    module O = ImplementOfInt(IntRationalFieldWithoutOfInt)
    let of_int = O.of_int
end

module FloatRationalFieldWithoutOfInt = RationalizeField(FloatField)

module FloatRational: Field = struct
    include FloatRationalFieldWithoutOfInt
    module O = ImplementOfInt(FloatRationalFieldWithoutOfInt)
    let of_int = O.of_int
end


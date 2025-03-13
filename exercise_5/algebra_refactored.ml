(***** Exercise: refactor arith *****)
(* This is the best solution I can write, several references are listed here:
 * 1. the use of `:=`: https://ocaml.org/manual/5.3/signaturesubstitution.html
 * 2. why so many intermediate modules: 
 *    - My question on reddit: https://www.reddit.com/r/ocaml/comments/1j9p0e6/need_help_on_one_ocaml_exercise_about_module/
 *    - Inspiration from featurewit001: https://github.com/featherwit001/CS3110_OCaml_Programming/blob/main/feather/exercises/algerbra.ml
 * I think a simple rule of thumb is that Prepare everything before doing final module type "CASTING".
 *)
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


module type Ring = sig
    include OfInt

    include RingWithoutOfInt with type t := t
end


module type Field = sig
    include Ring

    val ( / ): t -> t -> t
end


module ImplementOfInt (M: RingWithoutOfInt) = struct
    include M 

    let rec of_int x = 
        if x > 0 then ( + ) zero (of_int (Stdlib.(-) x 1))
        else if x < 0 then ( + ) zero (of_int (Stdlib.(+) x 1))
        else zero
end


module IntRingWithoutOfInt = struct
    type t = int

    let zero = 0

    let one = 1

    let ( + ) = ( + )

    let ( ~- ) = ( ~- )

    let ( * ) = ( * )

    let to_string = string_of_int
end


module FloatRingWithoutOfInt = struct
    type t = float

    let zero = 0.

    let one = 1.

    let ( + ) = ( +. )

    let ( ~- ) = ( ~-. )

    let ( * ) = ( *. )

    let to_string = string_of_float
end


module IntRingWithOfInt = ImplementOfInt(IntRingWithoutOfInt)


(*** IntRing: Ring ***)
module IntRing: Ring = IntRingWithOfInt


module FloatRingWithOfInt = ImplementOfInt(FloatRingWithoutOfInt)


(*** FloatRing: Ring ***)
module FloatRing: Ring = FloatRingWithOfInt


module IntRingWithOfIntEx = struct
    include IntRingWithOfInt

    let ( / ) = ( / )
end


(*** IntField: Field ***)
module IntField: Field = IntRingWithOfIntEx


module FloatRingWithOfIntEx = struct
    include FloatRingWithOfInt

    let ( / ) = ( /. )
end


(*** FloatField: Field ***)
module FloatField: Field = FloatRingWithOfIntEx


module RationalizeRingWithoutOfInt (M: RingWithoutOfInt) = struct
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


module IntRationalFieldWithoutOfInt = RationalizeRingWithoutOfInt(IntRingWithoutOfInt)


module IntRationalRingWithOfInt = ImplementOfInt(IntRationalFieldWithoutOfInt)


module IntRationalFieldWithOfInt = struct
    include IntRationalFieldWithoutOfInt

    let of_int = IntRationalRingWithOfInt.of_int
end


(*** IntRational: Field ***)
module IntRational: Field = IntRationalFieldWithOfInt


module FloatRationalFieldWithoutOfInt = RationalizeRingWithoutOfInt(FloatRingWithoutOfInt)


module FloatRationalRingWithOfInt = ImplementOfInt(FloatRationalFieldWithoutOfInt)


module FloatRationFieldWithOfInt = struct
    include FloatRationalFieldWithoutOfInt

    let of_int = FloatRationalRingWithOfInt.of_int
end


(*** FloatRational: Field ***)
module FloatRational: Field = FloatRationFieldWithOfInt


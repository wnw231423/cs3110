(*** Exercise: refactor arith ***)
module type Ring = sig
    type t

    val zero : t

    val one : t

    val ( + ) : t -> t -> t

    val ( ~- ) : t -> t

    val ( * ) : t -> t -> t

    val to_string : t -> string

    val of_int : int -> t
end

module IntRing: Ring = struct
    type t = int

    let zero = 0

    let one = 1

    let ( + ) = Stdlib.( + )

    let ( ~- ) = Stdlib.( ~- )

    let ( * ) = Stdlib.( * )

    let to_string = Int.to_string

    let of_int x = x
end

module FloatRing: Ring = struct
    type t = float

    let zero = 0.0

    let one = 1.0

    let ( + ) = Stdlib.( +. )

    let ( ~- ) = Stdlib.( ~-. )

    let ( * ) = Stdlib.( *. )

    let to_string = Float.to_string

    let of_int = Float.of_int
end

module RationalizeRing (M: Ring) = struct
    type num = M.t * M.t

    let zero = (M.zero, M.one)

    let one = (M.one, M.one)

    let ( + ) n1 n2 =
        match n1, n2 with
        | (nu1, de1), (nu2, de2) -> 
            ((M.( + ) (M.( * ) nu1 de2) (M.( * ) nu2 de1)), M.( * ) de1 de2)

    let ( ~- ) (nu, de) = (M.(~-) nu, de)

    let ( * ) n1 n2 = 
        match n1, n2 with
        | (nu1, de1), (nu2, de2) -> 
            ((M.( * ) nu1 nu2), M.( * ) de1 de2)

    let to_string (nu, de) = M.to_string nu ^ "/" ^ M.to_string de

    let of_int x = (M.of_int x, one)
end

module IntRationalRing = RationalizeRing(IntRing)

module FloatRationalRing = RationalizeRing(FloatRing)

module type Field = sig
    include Ring

    val ( / ): t -> t -> t
end

module IntField: Field = struct
    type t = int

    let zero = 0

    let one = 1

    let ( + ) = Stdlib.( + )

    let ( ~- ) = Stdlib.( ~- )

    let ( * ) = Stdlib.( * )

    let to_string = Int.to_string

    let of_int x = x

    let ( / ) = ( / )
end


(* Prelogue: This part requires more experience on coding rather than correctness.
        Referring to the official solution is much better than mines. I just write my
        initail self-written version ,refer to the official solution, make comparison
        and do self-summary. *)


(* Exercise: spec game
 * Simply referred to the official solution since no partner:(
 *)
module type SpecGame = sig
    (* [num_vowels s] returns the number of vowels in [s] *)
    val num_vowels: string -> int

    (* [is_sorted lst] returns true if [lst] is sorted ascendingly.
     * Requires: [compare] of ['a]
     *)
    val is_sorted: 'a list -> bool

    (* [sort l] returns [l'] which is a permutation of [l] and
     * [is_sorted l = true]
     *)
    val sort: 'a list -> 'a list

    (* [max l] returns [x] which is the biggest element in [l]
     * Requires: [l] is not empty.
     *)
    val max: 'a list -> 'a

    (* [is_prime x = true] if x is prime
     * Requires: [x > 0]
     *)
    val is_prime: int -> bool
end

(*****************************************)
(*** Exercise: poly spec and poly impl ***)
(*****************************************)

(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
    (** [t] is the type of polynomials. *)
    type t

    (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
        $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
    val eval: int -> t -> int

    (** [create_term co de] returns a term which is also a polynomial. 
        Example: [eval -5 3] returns $-5x^3$, where -5 is the coefficient
            and 3 is the degree. 
        Require: [de >= 0]*)
    val create_term: int -> int -> t

    (** [combine_polys t1 t2] takes in two polynomials, combine them 
            into one polynomial by addition and also combine like terms.
        Example: if t1 represents $3x^3+x^2$ and t2 represents $x^2+x$, then
            [combine t1 t2] represents $3x^3+2x^2+x *)
    val combine_polys: t -> t -> t

    (** [string_of_poly t] returns a user readable string of a polynomial.
        Terms are ordered by degree descendingly.*)
    val string_of_poly: t -> string
end

module LinkedPoly:Poly = struct
    type t = Nil | Term of int * int * t

    let rec pow x d = if d = 0 then 1 else x * (pow x (d - 1))

    let rec eval x t =
        match t with
        | Nil -> 0
        | Term(co, de, tt) -> (co * (pow x de) + (eval x tt))

    let create_term co de =
        if de < 0 then failwith "Expect degree >= 0"
        else if co = 0 then Nil
        else Term(co, de, Nil)

    let rec combine_polys t1 t2 =
        match t1, t2 with
        | Nil, _ -> t2
        | _, Nil -> t1
        | Term(co1, de1, p1), Term(co2, de2, p2) ->
            if de1 > de2 then Term(co1, de1, combine_polys t1 t2)
            else if de1 = de2 then 
                if co1 + co2 = 0 then combine_polys p1 p2 else Term(co1 + co2, de1, combine_polys p1 p2)
            else Term(co2, de2, combine_polys t1 p2)

    let rec string_of_poly = function
        | Nil -> "0"
        | Term(co, 0, Nil) -> string_of_int co 
        | Term(co, de, Nil) -> string_of_int co ^ "x^" ^ string_of_int de
        | Term(co, 1, t) -> string_of_int co ^ "x" ^ " + " ^ string_of_poly t
        | Term(co, de, t) -> string_of_int co ^ "x^" ^ string_of_int de ^ " + " ^ string_of_poly t 
end

(** Summary:
    1. Interface.
        1. Construction: Complex but at least ok from the perspective of just interface.
        2. Querying: Disaster. I totally didn't get the idea of Querying. I should provide
            some methods like [coefficient] and [degree].
        3. Operation: Only [add] is provided. I have to admit that I didn't think too much
            and the [add] operation is the only operation I could immediately come up with
    2. Implementaiton.
        1. Coefficient list is a much better choice than my linkedList
            implelmentation. That can make [construciton] and [add] much easier than mines,
            let alone his extra queryings and operations
        2. His implementation doc did so well while I totally comment nothing. *)


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

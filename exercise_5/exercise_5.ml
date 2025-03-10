(*** Exercise: complex synonym ***)
module type ComplexSig = sig
    type t = float * float
    val zero: t
    val add: t -> t -> t
end


(*** Exercise: complex encapsulation ***)
(* 1. remove [zero] from the structure, not implementing [zero], error
 * 2. remove [add] from the signature, no error
 * 3. change [zero] to interger tuple, doesn't match signature, error
 *)


(*** Exercise: big list queue ***)
module ListQueue = struct
    type 'a queue = 'a list

    let empty = []

    let is_empty q = (q = [])

    let enqueue x q = q @ [x]

    let peek = function
        | [] -> failwith "Empty"
        | x::_ -> x

    let dequeue = function
        | [] -> failwith "Empty"
        | _::q -> q
end

let fill_listqueue n =
    let rec loop n q =
        if n = 0 then q
        else loop (n - 1) (ListQueue.enqueue n q) in
    loop n ListQueue.empty
(* About 30000 will bring a delay of at least 10s. *)


(*** Exercise: big batched queue ***)
module BatchedQueue = struct
    type 'a t = {outbox: 'a list; inbox: 'a list}

    let empty = {outbox = []; inbox = []}

    let is_empty = function
        | {outbox = []; inbox = []} -> true
        | _ -> false

    let norm = function
        | {outbox = []; inbox} -> {outbox = List.rev inbox; inbox = []}
        | q -> q

    let enqueue x q = norm {q with inbox = x :: q.inbox}

    let peek = function
        | {outbox = []; _} -> None
        | {outbox = x::_; _} -> Some x

    let dequeue = function
        | {outbox = []; _} -> None
        | {outbox = _::tail; inbox} -> Some (norm {outbox = tail; inbox})
end

let fill_batchedqueue n = 
    let rec loop n q =
        if n=0
        then q
        else loop (n-1) (BatchedQueue.enqueue n q)
    in
    loop n BatchedQueue.empty
(* 100_000_000 brings a delay of less than 10s, 200_000_000 crushes my memory. *)


(*** Exercise: queue efficiency ***)
(* 1. List is implemented by single linked list. In ListQueue, when do enqueue,
 *    you need to append the element at the end of list, which takes linear time.
 * 2. Adding n elements take 1 + 2 + ... + n time, which is O(n^2).
 * 3. In BatchedQueue, since inbox regards head as the last position of queue, 
 *    adding one element at the head of a list takes constant time.
 * 4. Adding n elements takes n * Constant, which is linear. 
 *)



(*** Exercise: binary search tree map ***)
module type Map = sig
    type ('k, 'v) t
    val empty: ('k, 'v) t
    val insert: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
    val lookup: 'k -> ('k, 'v) t -> 'v
    val bindings: ('k, 'v) t -> ('k * 'v) list
end

module BstMap: Map = struct
    type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
    
    type ('k, 'v) t = ('k * 'v) tree
    
    let empty = Leaf
    
    (* Comments inside are my bad codes... *)
    let rec insert k v = function
        | Leaf -> Node ((k, v), Leaf, Leaf)
        | Node ((key, value), left, right) ->
            if k = key then Node ((key, v), left, right)
            (* else if k < key then insert k v left *)
            else if k < key then Node ((key, value), insert k v left, right)
            (* else insert k v right *)
            else Node ((key, value), left, insert k v right)

    let rec lookup k = function
        | Leaf -> failwith "key not found"
        | Node ((key, value), left, right) ->
            if (key = k) then value
            else if k < key then lookup k left
            else lookup k right

    let rec bindings = function
        | Leaf -> []
        | Node ((k, v), left, right) -> 
            bindings left @ [(k, v)] @ bindings right
end


(*** Exercise: fraction ***)
module type Fraction = sig
    type t
    val make: int -> int -> t
    val numerator: t -> int
    val denominator: t -> int
    val to_string: t -> string
    val to_float: t -> float
    val add: t -> t -> t
    val mul: t -> t -> t
end

module Fraction: Fraction = struct
    type t = Fr of int * int

    let make n d =
        if d = 0 then failwith "zero division error"
        else Fr (n, d)
    
    let numerator = function
        | Fr (n, _) -> n

    let denominator = function
        | Fr (_, d) -> d

    let to_string = function
        | Fr (n, d) -> string_of_int n ^ "/" ^ string_of_int d

    let to_float = function
        | Fr (n, d) -> Float.of_int n /. Float.of_int d

    let add f1 f2 = 
        match f1, f2 with
        | Fr (n1, d1), Fr (n2, d2) -> Fr (n1 * d2 + n2 * d1, d1 * d2)

    let mul f1 f2 = 
        match f1, f2 with
        | Fr (n1, d1), Fr (n2, d2) -> Fr (n1 * n2, d1 * d2)
end

module ReduceFranction (M: Fraction) = struct
    include M

    (** [gcd x y] is the greatest common divisor of [x] and [y].
    Requires: [x] and [y] are positive. *)
    let rec gcd x y =
        if x = 0 then y
        else if (x < y) then gcd (y - x) x
        else gcd y (x - y)

    let reduce v = 
        let n = numerator v in
        let d = denominator v in
        let gcd_num = gcd (abs n) (abs d) in
        if d > 0 then make (n / gcd_num) (d / gcd_num)
        else make (-n / gcd_num) (-d / gcd_num)

    let make n d = M.make n d |> reduce

    let add f1 f2 = M.add f1 f2 |> reduce 

    let mul f1 f2 = M.mul f1 f2 |> reduce 
end 

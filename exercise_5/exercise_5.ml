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


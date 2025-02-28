(* Exercise: patterns *)
let bigred = function
    | "bigred"::_ -> true
    | _ -> false

let two_or_four = function
    | _::_::[] | _::_::_::_::[] -> true
    | _ -> false

let first_two_equal = function
    | first::second::_ -> first = second
    | _ -> false


(* Exercise: library *)
let fifth (lst: int list) = 
    if List.length lst < 5 then 0 else List.nth lst 4

let des_sort (lst: int list) = 
    let sorted = List.sort Stdlib.compare lst
    in List.rev sorted


(* Exercise: library puzzle *)
let last_element lst = 
    List.hd (List.rev lst)

let any_zeros (lst: int list): bool = 
    let f x = x = 0 in
    if (List.find_opt f lst) = None then false else true


(* Exercise: take drop *)
let take n lst = 
    let rec take_helper n acc lst= 
        match lst with
        | [] -> acc
        | head::tail -> if n = 0 then acc else take_helper (n - 1) (acc@[head]) tail
    in take_helper n [] lst

let rec drop n lst = 
    match lst with
    | [] -> []
    | _::tail -> if n = 0 then lst else drop (n - 1) tail
     

(* Exercies: unimodal *)
let is_unimodal lst =
    let rec is_unimodal_helper last state lst =
        if state = 1 then
            match lst with
            | [] -> true
            | head::tail ->
                match last with
                | None -> is_unimodal_helper (Some head) 1 tail
                | Some x -> if head > x then is_unimodal_helper (Some head) 1 tail
                            else is_unimodal_helper (Some head) 2 tail
        else
            match lst with
            | [] -> true
            | head::tail -> 
                match last with
                | None -> failwith "Unexpected None"
                | Some x -> if head > x then false else
                    is_unimodal_helper (Some head) 2 tail
    in is_unimodal_helper None 1 lst


(* Exercise: powerset *)
let rec powerset lst = 
    match lst with
    | [] -> [[]]  (* Note that the powerset of [] is [[]] *)
    | head::tail -> 
        let insert x l = x::l
        in (List.map (insert head) (powerset tail))@(powerset tail)


(* Exercise: pokefun *)
type poketype = Normal | Fire | Water
type pokemon = {name:string; hp:int; ptype:poketype}

let charizard = {name = "charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "squirtle"; hp = 44; ptype = Water}

let safe_hd = function
    | [] -> None
    | head::_ -> Some head

let rec safe_tl = function
    | [] -> None
    | [x] -> Some x
    | _::tail -> safe_tl tail

let rec max_hp = function
    | [] -> None
    | head::tail ->
        match max_hp tail with
        | None -> Some head
        | Some p -> Some (if head.hp >= p.hp then head else p)


(* Exercise: earliest date *)
type date = int * int * int

let is_before date1 date2 =
    let (y1, m1, d1) = date1 in
    let (y2, m2, d2) = date2 in
    if y1 < y2 then true
    else if y1 = y2 && m1 < m2 then true
    else if y1 = y2 && m1 = m2 && d1 < d2 then true
    else false

let rec earliest date_lst = 
    match date_lst with
    | [] -> None
    | head::tail ->
        match earliest tail with
        | None -> Some head
        | Some x -> if is_before head x then Some head else Some x


(* Exercise: shape *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec same_shape tree1 tree2 =
    match (tree1, tree2) with
    | (Leaf, Leaf) -> true
    | (Node (_, left1, right1), Node (_, left2, right2)) ->
        same_shape left1 left2 && same_shape right1 right2 
    | _ -> false


(* Exercise: is_bst *)

(* Suppose a way to compare tuple *)
let compare_tuple (x1, y1) (x2, y2) =
    if x1 < x2 then -1
    else if x1 = x2 && y1 < y2 then -1
    else if x1 = x2 && y1 = y2 then 0
    else 1

type 'a validation = Empty | Invalid | Valid of ('a * 'a)

let is_bst (t: ('a * 'b) tree) =
    let rec is_bst_helper t =
        match t with
        | Leaf -> Empty
        | Node (value, left_tree, right_tree) ->
            match is_bst_helper left_tree, is_bst_helper right_tree with
            | Invalid, _ -> Invalid
            | _, Invalid -> Invalid
            | Empty, Valid (right_max, right_min) ->
                if compare_tuple value right_min <= 0 then Valid (right_max, value)
                else Invalid
            | Valid (left_max, left_min), Empty ->
                if compare_tuple left_max value <= 0 then Valid (value, left_min)
                else Invalid
            | Valid (left_max, left_min), Valid (right_max, right_min) ->
                if compare_tuple value right_min <= 0 && compare_tuple left_max value <= 0
                    then Valid (right_max, left_min)
                else Invalid
            | Empty, Empty -> Valid (value, value)
    in match is_bst_helper t with
    | Empty | Valid (_, _) -> true
    | Invalid -> false


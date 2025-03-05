(*** Exercise: account balance ***)
(* fold left version *)
let account_fl debit account =
    List.fold_left (-) account debit

(* fold right version *)
let account_fr debit account =
    List.fold_right (fun x y -> y - x) debit account

(* directly recursive version *)
let rec account_dr debit account =
    match debit with
    | [] -> account
    | h::t -> account_dr t (account - h)


(*** Exercise: map composition ***)
let two_map f g lst = List.map f (List.map g lst)
let one_map f g lst = List.map (fun x -> f g x) lst


(*** Exercise: more list fun ***)
let mlf_1 (lst: string list) =
    List.filter (fun s -> String.length s > 3) lst

let mlf_2 (lst: float list) =
    List.map (fun f -> f +. 1.0) lst

let mlf_3 strs sep =
    match strs with
    | [] -> ""
    | h::t -> List.fold_left (fun s1 s2 -> s1 ^ sep ^s2) h t


(*** Exercise: association list keys ***)
let keys (lst: ('a * 'b) list) =
    List.fold_right (fun (x, _) l -> x::l) lst []
    |> List.sort_uniq compare


(*** Exercise: valid matrix ***)
let validate_matrix = function
    | [] -> false
    | h::t ->
        let cols = List.length h in
        List.fold_left (fun b l -> b && List.length l = cols) true t


(*** Exercise: row vector add ***)
let add_row_vector v1 v2 =
    match v1, v2 with
    | [], _ -> v2
    | _, [] -> v1
    | _, _ -> List.map2 (fun x y -> x + y) v1 v2


(*** Exercise: matrix add ***)
let add_matrices m1 m2 = 
    List.map2 add_row_vector m1 m2


(*** Exercise: matrix multiply ***)
(* This function aims to transpose a nx1 vector into a 1xn vector *)
let rec transpose_vector = function
    | [] -> []
    | head::tail -> [head]::transpose_vector tail

(* This function aims to combine two 1xn vector into a 2xn matrix *)
let combine_col_vector v1 v2 =
    match v1, v2 with
    | [], _ -> v2
    | _, [] -> v1
    | _, _ -> List.map2 (fun lst1 lst2 -> lst1 @ lst2) v1 v2

let transpose_matrix m =
    List.fold_left (combine_col_vector) [] (List.map transpose_vector m)

let dot_product v1 v2 =
    List.map2 (fun x y -> x * y) v1 v2 |> List.fold_left (+) 0

(* This functon aims to multiply a 1xn vector with a nxk matrix *)
let vector_matrix_product v m =
    List.map ((fun v1 v2 -> dot_product v1 v2) v) (transpose_matrix m)

let rec multiply_matrices m n =
    match m with
    | [] -> []
    | head::tail -> (vector_matrix_product head n)::(multiply_matrices tail n)


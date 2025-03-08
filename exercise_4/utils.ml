open OUnit2

let string_of_int_list lst =
    "[" ^ String.concat ";" (List.map string_of_int lst) ^ "]"

let string_of_matrix lst =
    "[" ^ String.concat ";" (List.map string_of_int_list lst) ^ "]"

let maketest name expected fx printer = 
    name >:: (fun _ -> assert_equal expected fx ~printer:printer)

let run_test = run_test_tt_main

open OUnit2
open Exercise_4
open Utils

let invalid_matrix = [[1;2;3];[4;5]]
let valid_matrix = [[1;2;3;4];[3;4;5;6];[2;3;4;5];[1;2;3;4];[1;2;3;5]]
let test1 = "test suite for valid matrix" >:::[
    maketest "empty" false (validate_matrix []) string_of_bool;
    maketest "invalid" false (validate_matrix invalid_matrix) string_of_bool;
    maketest "valid" true (validate_matrix valid_matrix) string_of_bool
]

let vector1 = [1;2;3;4]
let vector2 = [1;3;5;7]
let test2 = "test suite for add_row_vector" >:::[
    maketest "right" [2;5;8;11] (add_row_vector vector1 vector2) string_of_int_list
]

let matrix1 = [[1;1;1];[1;1;1]]
let matrix2 = [[1;2;3];[4;5;6]]
(* let matrix3 = [] *)
let test3 = "test suite for add_matrices" >:::[
    maketest "right" [[2;3;4];[5;6;7]] (add_matrices matrix1 matrix2) string_of_matrix
]
let matrix4 = [[1;2];[3;4]]
let matrix5 = [[5;6];[7;8]]
let matrix6 = [[1;2;3];[4;5;6]]
let matrix7 = [[7;8];[9;10];[11;12]]
let test4 = "test suite for multiply_matrices" >:::[
    maketest "2x2 matrices" [[19;22];[43;50]] (multiply_matrices matrix4 matrix5) string_of_matrix;
    maketest "2x3 and 3x2 matrices" [[58;64];[139;154]] (multiply_matrices matrix6 matrix7) string_of_matrix;
]

let () = run_test (
    "all tests" >::: [
        test1;
        test2;
        test3;
        test4
    ]
)


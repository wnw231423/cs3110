open OUnit2
open Exercise_3

let make_patterns_test name expected f input = 
    name >:: (fun _ -> assert_equal expected (f input) ~printer:string_of_bool)

let tests = "test suite for patterns" >::: [
    make_patterns_test "bigred_false" false bigred ["hello";"world"];
    make_patterns_test "bigred_true" true bigred ["bigred"; "icecream"];
    make_patterns_test "two_or_four_false" false two_or_four [1;2;3];
    make_patterns_test "two_or_four_true" true two_or_four [1;2;4;3];
    make_patterns_test "first_two_equal_false" false first_two_equal [99; 98; 90];
    make_patterns_test "first_two_equal_true" true first_two_equal [99; 99; 90];
]

let make_library_test name expected f input = 
    name >:: (fun _ -> assert_equal expected (f input))

let tests2 = "test suit for library" >::: [
    make_library_test "fifth_false" 0 fifth [999; 999; 999; 999];
    make_library_test "fifth_true" 8 fifth [999; 999; 999; 999; 8];
    make_library_test "des_sort" [5;4;3;2;1] des_sort [3;4;5;2;1];
]


let make_test name expected input print_fun = 
    name >:: (fun _ -> assert_equal expected input ~printer: print_fun)

let tree1 = Node ((6, 1.0),
                Node ((4, 1.0),
                    Node ((2, 1.0),
                        Node ((1, 1.0), Leaf, Leaf),
                        Node ((3, 1.0), Leaf, Leaf)
                    ),
                    Node ((5, 1.0), Leaf, Leaf)
                ),
                Node ((8, 1.0),
                    Node ((7, 1.0), Leaf, Leaf),
                    Node ((9, 1.0), Leaf, Leaf)
                )
        )

let tree2 = Node ((6, 1.0),
                Node ((4, 1.0),
                    Node ((2, 1.0),
                        Node ((1, 1.0), Leaf, Leaf),
                        Node ((3, 1.0), Leaf, Leaf)
                    ),
                    Node ((5, 1.0), Leaf, Leaf)
                ),
                Node ((8, 1.0),
                    Node ((999, 1.0), Leaf, Leaf),
                    Node ((9, 1.0), Leaf, Leaf)
                )
        )

let tests3 = "test suite for is_bst" >::: [
    make_test "is_bst_true" true (is_bst tree1) string_of_bool;
    make_test "is_bst_false" false (is_bst tree2) string_of_bool
]

(******************************)
let () = run_test_tt_main tests
let () = run_test_tt_main tests2
let () = run_test_tt_main tests3

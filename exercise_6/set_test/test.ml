open OUnit2
open List_set
(*********************************************)
(*** Exercise: set black box and glass box ***)
(*********************************************)

(* producer: [empty], [add]
   consumer: [mem], [elements] *)

let list_set_test = 
    let open List_set.ListSet in
    "black box test for list_set" >:::[
    "mem of empty" >:: (fun _ -> assert_equal false (mem 0 empty));
    "mem of add" >:: (fun _ -> assert_equal true (mem 0 (add 0 empty)));
    "elements of empty" >:: (fun _ -> assert_equal [] (elements empty));
    "elements of add without duplication" >:: (fun _ -> assert_equal 2 (List.length (elements (add 1 (add 2 empty)))));
    "elements of add with duplication" >:: (fun _ -> assert_equal 1 (List.length (elements (add 1 (add 1 empty)))));
    ]

let uniquelist_set_test = 
    let open List_set.UniqListSet in
    "glass box test for list_set" >:::[
    "mem of empty" >:: (fun _ -> assert_equal false (mem 0 empty));
    "mem of add" >:: (fun _ -> assert_equal true (mem 0 (add 0 empty)));
    "elements of empty" >:: (fun _ -> assert_equal [] (elements empty));
    "elements of add without duplication" >:: (fun _ -> assert_equal 2 (List.length (elements (add 1 (add 2 empty)))));
    "elements of add with duplication" >:: (fun _ -> assert_equal 1 (List.length (elements (add 1 (add 1 empty)))));
]

let tests = 
    "all tests" >::: [list_set_test; uniquelist_set_test]

let _ = run_test_tt_main tests

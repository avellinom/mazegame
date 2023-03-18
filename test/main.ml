open OUnit2

let default_test (name : string) (input : string) : test =
  name >:: fun _ ->
  assert_equal "hi" input


let default_tests = [ 
  default_test "default_test" "hi";
]


 let tests =
  "test suite for MS2"
  >::: List.flatten
         [
           default_tests;
         ]

let _ = run_test_tt_main tests

open Core

let to_string_list str = String.to_list str |> List.map ~f:String.of_char

let look_and_say chrs = 
  List.group chrs ~break:(fun a b -> a <> b) 
  |> List.bind ~f:(function | [] -> [] | x :: xs -> [List.length xs + 1 |> Int.to_string; x])

let%test_unit _ =
  [%test_result : string list] 
    (look_and_say (to_string_list "111221")) ~expect:(to_string_list "312211")

let rec apply_n f n x =
  if n <= 0 then
    x
  else 
    apply_n f (n - 1) (f x)

let%expect_test _ = 
  apply_n look_and_say 40 (to_string_list "1113222113") |> List.length |> Int.to_string |> print_endline;
  [%expect {| 252594 |}]

let%expect_test _ = 
  apply_n look_and_say 50 (to_string_list "1113222113") |> List.length |> Int.to_string |> print_endline;
  [%expect {| 3579328 |}]
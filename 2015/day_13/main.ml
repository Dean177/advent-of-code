open Core

let puzzle_input = In_channel.read_lines "./input.txt"

let%expect_test _ =
  print_endline "Part One";
  [%expect {|
    Part One |}]
    
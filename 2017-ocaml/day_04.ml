open Core
open Toolbox

let input = 
  In_channel.read_all "./day_04.txt" 
  |> String.split_lines
  |> List.map ~f:String.words

let isValidPassphrase words = 
  let unique = String.Set.of_list words in 
  Set.length unique = List.length words

let isValidLine words = 
  List.map words ~f:(fun word -> 
      String.to_list word
      |> List.sort ~compare:(Char.compare)
      |> String.of_char_list
    )
  |> isValidPassphrase 

let%expect_test _ =
  print_endline "Part 1";
  List.filter input ~f:isValidPassphrase
  |> List.length
  |> Int.to_string
  |> print_endline;

  print_endline "Part 2";
  List.filter input ~f:isValidLine
  |> List.length
  |> Int.to_string
  |> print_endline;

  [%expect {|
    Part 1
    451
    Part 2
    223 |}]

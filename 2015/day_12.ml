open Core
open Yojson

let puzzle_input = 
  Basic.from_file "./day_12.json"

let rec numeric_values = function
  | `Int i -> [i]
  | `List jsons -> List.bind jsons ~f:numeric_values
  | `Assoc obj -> List.bind obj ~f:(fun (_, jsons) -> numeric_values jsons)
  | _ -> []

let%expect_test _ =
  puzzle_input |> numeric_values |> List.fold ~f:( + ) ~init:0 |> [%sexp_of : int] |> print_s;
  [%expect {|
    156366 |}]

let has_red obj = List.exists obj ~f:(fun (_, jsons) -> match jsons with 
    | `String "red" -> true
    | _ -> false
  )

let rec non_red_numeric_values = function
  | `Int i -> [i]
  | `List jsons -> List.bind jsons ~f:non_red_numeric_values
  | `Assoc obj -> 
    if has_red obj then [] else
      List.bind obj ~f:(fun (_, jsons) -> non_red_numeric_values jsons)
  | _ -> []

let%expect_test _ =
  puzzle_input |> non_red_numeric_values |> List.fold ~f:( + ) ~init:0 |> [%sexp_of : int] |> print_s;
  [%expect {|
    96852 |}]

let sum = List.sum
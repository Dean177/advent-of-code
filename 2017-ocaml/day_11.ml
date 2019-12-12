open Core
open Toolbox

let step (x, y, z) = function 
  | "n" -> (x, y + 1, z - 1)
  | "ne" -> (x + 1, y, z - 1)
  | "se" -> (x + 1, y - 1, z)
  | "s" -> (x, y - 1, z + 1)
  | "sw" -> (x - 1, y, z + 1)
  | "nw" -> (x - 1, y + 1, z)
  | s -> failwith ("Unrecognised string: " ^ s)

let distance (x, y, z) = Int.((abs x + abs y + abs z) / 2)

let%expect_test _ =
  let input =  In_channel.read_all "./day_11.txt" |> String.split ~on:',' in

  print_endline "Part 1";
  List.fold ~init:(0, 0, 0) ~f:step input 
  |> distance 
  |> Int.to_string
  |> print_endline; 

  print_endline "Part 2";
  List.scan ~init:(0, 0, 0) ~f:step input
  |> List.map ~f:distance
  |> List.max_elt ~compare:Int.compare 
  |> Option.value_exn
  |> Int.to_string
  |> print_endline;

  [%expect {|
    Part 1
    805
    Part 2
    1535 |}]

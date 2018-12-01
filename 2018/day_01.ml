open Core
open Toolbox

let puzzleInput = 
  In_channel.read_all "./day_01.txt" 
  |> String.split_lines
  |> List.map ~f:Int.of_string

let%expect_test _ =  
  print_endline "Part 1";
  Int.List.sum puzzleInput |> Int.to_string  |> print_endline;

  print_endline "Part 2";
  Sequence.cycle_list_exn puzzleInput 
  |> Sequence.fold_until_exn
    ~init:(Int.Set.empty, 0) 
    ~f:(fun (seen, current) change -> 
        let frequency = current + change in
        if Set.mem seen frequency then 
          Stop frequency
        else 
          Continue ((Set.add seen frequency), frequency)
      )
  |> Int.to_string  
  |> print_endline; 
  [%expect {|
    Part 1    
    493
    Part 2 
    413
  |}]

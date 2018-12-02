open Core
open Toolbox

let puzzleInput = 
  In_channel.read_all "./input.txt" 
  |> String.split_lines


let characterCounts (boxId: string) : int Char.Map.t = 
  let letters = String.to_array boxId in
  Array.fold letters ~init:Char.Map.empty ~f:(fun characterCounts character ->
      Map.update characterCounts character ~f:(function | None -> 1 | Some count -> count + 1)
    )


let partOne (boxIds) = 
  List.map boxIds ~f:characterCounts
  |> List.fold ~init:(0,0) ~f:(fun (twos, threes) characterCount -> 
      let containsTwo = Map.exists characterCount ~f:(fun count -> count = 2) in
      let containsThree = Map.exists characterCount ~f:(fun count -> count = 3) in
      ((if containsTwo then twos + 1 else twos), (if containsThree then threes + 1 else threes))
    )

let differenceCharacterCount a b =  
  let aChars = String.to_array a in
  let bChars = String.to_array b in
  Array.zip_exn aChars bChars 
  |> Array.fold ~init:0 ~f:(fun count (ac, bc) -> if ac <> bc then count + 1 else count)


let rec partTwo boxIds = match boxIds with
  | [] | _ :: [] -> None
  | a :: rest -> (
      match List.find rest ~f:(fun b -> differenceCharacterCount a b = 1) with
      | Some b -> Some (a, b)
      | None -> partTwo rest
    )


let%expect_test _ =  
  print_endline "Part 1";
  let (twos, threes) = partOne puzzleInput in
  let checksum = twos * threes in
  print_endline (Int.to_string checksum);
  print_endline "Part 2";
  partTwo puzzleInput 
  |> Option.map ~f:(fun (a, b) -> 
      List.zip_exn (String.to_list a) (String.to_list b) 
      |> List.filter_map ~f:(fun (ac, bc) -> if ac = bc then Some ac else None) 
      |> String.of_char_list
    ) 
  |> Option.value_exn
  |> print_endline;
  [%expect {|
    Part 1    
    7808
    Part 2 
    efmyhuckqldtwjyvisipargno
  |}]

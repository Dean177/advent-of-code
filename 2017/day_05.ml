open Core

let puzzleInput = In_channel.read_all "./day_05.txt" 

let parse input = 
  input
  |> String.split_lines
  |> List.map ~f:Int.of_string
  |> Array.of_list

let rec partOne input ~instruction ~total =   
  if instruction < 0 || instruction >= Array.length input then
    total 
  else (
    let next_instruction = instruction + input.(instruction) in
    input.(instruction) <- input.(instruction) + 1;
    partOne input ~instruction:next_instruction ~total:(total + 1)
  )

let rec partTwo input ~instruction ~total =   
  if instruction < 0 || instruction >= Array.length input then
    total 
  else (
    let jump_distance = input.(instruction) in
    let next_instruction = instruction + jump_distance in
    let increment = if jump_distance >= 3 then -1 else 1 in
    input.(instruction) <- input.(instruction) + increment;
    partTwo input ~instruction:next_instruction ~total:(total + 1)
  )

let%expect_test _ =
  print_endline "Part One";
  partOne (parse puzzleInput) ~instruction:0 ~total:0
  |> Int.to_string 
  |> print_endline;

  print_endline "Part Two";
  partTwo (parse puzzleInput) ~instruction:0 ~total:0
  |> Int.to_string 
  |> print_endline;

  [%expect {|
    Part One
    387096
    Part Two
    28040648 |}]

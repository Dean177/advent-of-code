open Core

let puzzleInput = 
  In_channel.read_all "./input.txt"

let part1 chars = 
  List.fold_right chars ~init:[] ~f:(fun x chars -> match chars with 
  | y :: ys when x <> y && Char.uppercase x = Char.uppercase y -> ys
  | ys -> x :: ys ) 
  |> List.length

let partTwo str = 
  let uppercaseCharacters = 
    String.uppercase str
    |> String.to_list 
    |> Char.Set.of_list 
    |> Set.to_list  
  in
  let input = String.to_list str in
  List.fold uppercaseCharacters ~init:Int.max_value ~f:(fun minimumPolyLength polymer ->
    let instance = List.filter input ~f:(fun c -> 
      c <> polymer && c <> Char.lowercase polymer) 
    in
    let polymerLength = part1 instance in
    Int.min minimumPolyLength polymerLength
  )

let%expect_test _ =  
  print_endline "Part 1";
  part1 (String.to_list puzzleInput) |> Int.to_string |> print_endline;

  print_endline "Part Two";
  partTwo puzzleInput |> Int.to_string |> print_endline;

  [%expect {|
    Part 1
    11814
    Part Two
    4282
  |}]

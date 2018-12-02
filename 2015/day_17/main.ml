open Core

let testInput = "20
15
10
5
5"

let puzzleInput = "50
44
11
49
42
46
18
32
26
40
21
7
18
43
10
47
36
24
22
40"

let containers str = 
  String.split_lines str 
  |> List.map ~f:Int.of_string 
  |> List.sort ~compare:Int.compare 
  |> List.rev

let rec countCombinations (space: int) (containers: int list) = 
  match containers with 
  | [] -> if space = 0 then 1 else 0
  | container :: rest -> 
    countCombinations space rest + countCombinations (space - container) rest

let rec combinations (space: int) (containers: int list) (currentCount : int) (acc : int list)= 
  match containers with 
  | [] -> if space = 0 then currentCount :: acc else acc 
  | container :: rest -> 
    List.concat [
      (combinations space rest currentCount acc);
      (combinations (space - container) rest (currentCount + 1) acc);
    ]

let%expect_test _ = 
  print_endline "Part One";
  countCombinations 150 (containers puzzleInput) |> [%sexp_of : int] |> print_s;

  let combos = combinations 150 (containers puzzleInput) 0 [] in 
  let minContainers = List.min_elt combos ~compare:Int.compare |> Option.value_exn in
  print_endline "Part One";
  List.length (List.filter combos ~f:(fun containerCount -> containerCount = minContainers)) |> [%sexp_of : int] |> print_s;

  [%expect {|
    Part One
    654
    Part One
    57 |}]


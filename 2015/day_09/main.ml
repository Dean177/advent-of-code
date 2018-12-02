open Core

let puzzleInput = "Tristram to AlphaCentauri = 34
Tristram to Snowdin = 100
Tristram to Tambi = 63
Tristram to Faerun = 108
Tristram to Norrath = 111
Tristram to Straylight = 89
Tristram to Arbre = 132
AlphaCentauri to Snowdin = 4
AlphaCentauri to Tambi = 79
AlphaCentauri to Faerun = 44
AlphaCentauri to Norrath = 147
AlphaCentauri to Straylight = 133
AlphaCentauri to Arbre = 74
Snowdin to Tambi = 105
Snowdin to Faerun = 95
Snowdin to Norrath = 48
Snowdin to Straylight = 88
Snowdin to Arbre = 7
Tambi to Faerun = 68
Tambi to Norrath = 134
Tambi to Straylight = 107
Tambi to Arbre = 40
Faerun to Norrath = 11
Faerun to Straylight = 66
Faerun to Arbre = 144
Norrath to Straylight = 115
Norrath to Arbre = 135
Straylight to Arbre = 127"

module StringTuple = struct
  module T = struct
    type t = string * string [@@ deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)

end

let parse input =
  let lines = String.split_lines input in
  let (distances, locations) = 
    List.fold lines ~init:(StringTuple.Map.empty, String.Set.empty) ~f:(fun (journeys, locations) line ->
        match (Toolbox.String.words(line)) with 
        | origin :: _ :: destination :: _ :: distance_string :: [] -> 
          let distance = Int.of_string(distance_string) in
          let journeys' = 
            journeys
            |> Map.set  ~key:(origin, destination) ~data:distance
            |> Map.set  ~key:(destination, origin) ~data:distance
          in
          (journeys', Set.add (Set.add locations origin) destination)
        | _ -> failwith ("Malformatted input: " ^ line)
      )
  in
  (distances, Set.to_list locations)

let rec distance (distances: int StringTuple.Map.t) (route: string list) : int = 
  match route with 
  | origin :: destination :: rest -> (
      Map.find_exn distances (origin, destination) + distance distances (destination :: rest)
    )
  | _ :: [] | [] -> 0

let%expect_test _ = 
  let (distances, locations) = parse puzzleInput in
  let routes = 
    Toolbox.List.permutations locations 
    |> Sequence.to_list 
    |> List.map ~f:(fun (route: string list) -> 
        (String.concat ~sep:" -> " route, distance distances route)
      ) in

  print_endline("Part 1");
  List.min_elt routes ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.value_exn 
  |> [%sexp_of : string * int] 
  |> print_s;

  print_endline("Part 2");
  List.max_elt routes ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.value_exn |> [%sexp_of : string * int] |> print_s;

  [%expect {|
    Part 1
    ("Tambi -> Arbre -> Snowdin -> AlphaCentauri -> Tristram -> Straylight -> Faerun -> Norrath"
     251)
    Part 2
    ("Tristram -> Faerun -> Arbre -> Straylight -> AlphaCentauri -> Norrath -> Tambi -> Snowdin"
     898)|}]

open Core

type sue = int * ((string * int) list) [@@deriving sexp]

module Parser = struct
  open Angstrom
  open Toolbox.Parser
  (* Sue 464: akitas: 10, goldfish: 10, trees: 1 *)
  let pet = Tuple2.create <$> take_while (fun chr -> chr <> ':') <* string ": " <*> integer
  let pets = sep_by (string ", ") pet
  let sue = Tuple2.create <$> string "Sue " *> integer <* string ": " <*> pets
  let parse = parse_string_exn (sep_by_newline sue)
end

let real_sue = String.Map.of_alist_exn [
    ("children", 3);
    ("cats", 7);
    ("samoyeds", 2);
    ("pomeranians", 3);
    ("akitas", 0);
    ("vizslas", 0);
    ("goldfish", 5);
    ("trees", 3);
    ("cars", 2);
    ("perfumes", 1);
  ]

let sue_score pets = List.fold pets ~init:0 ~f:(fun total (pet_name, qty) ->
    match Map.find real_sue pet_name with 
    | Some pet_qty when pet_qty = qty -> total + 1
    | _ -> total
  )

let%expect_test "part 1" = 
  In_channel.read_all "./day_16.txt" 
  |> Parser.parse
  |> List.map ~f:(fun (sue_number, pets) -> (sue_number, sue_score pets))
  |> List.max_elt ~compare:(fun (_, score_a) (_, score_b) -> Int.compare score_a score_b)
  |> [%sexp_of : (int * int) option] |> print_s;
  [%expect {| ((103 3)) |}]


let sue_filter pets = List.for_all pets ~f:(fun (pet_name, qty) ->
    match Map.find real_sue pet_name with 
    | Some real_qty  -> 
      ((pet_name = "cats" || pet_name = "trees") && qty > real_qty) || 
      ((pet_name = "pomeranians" || pet_name = "goldgish") && qty < real_qty) ||
      (real_qty = qty) 
    | None -> true
  )

let%expect_test "part 2" = 
  In_channel.read_all "./day_16.txt" 
  |> Parser.parse
  |> List.filter ~f:(fun (_, pets) -> sue_filter pets)
  |> [%sexp_of : sue list] |> print_s;
  [%expect {|
    ((103 ((cars 2) (perfumes 1) (goldfish 5)))
     (405 ((trees 8) (perfumes 1) (cars 2)))) |}]

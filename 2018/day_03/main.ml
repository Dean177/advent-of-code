open Core

let puzzleInput = In_channel.read_all "./input.txt"

type claim = {
  id: int;
  position: int * int;
  area: int * int;
} [@@deriving sexp]

let makeClaim id position area = { id; position;area;}

module Parser = struct
  open Angstrom
  open Toolbox.Parser
  let id = char '#' *> integer
  let makeTuple a b = (a, b)
  let position = makeTuple <$> integer <*> (char ',' *> integer)
  let area = makeTuple <$> integer <*> (char 'x' *> integer)
  let claim = makeClaim <$> id <*> (string " @ " *> position) <*> (string ": " *> area)
  let parse = parse_string_exn (sep_by_newline claim)
end

let claims = Parser.parse puzzleInput

let partOne claims =
  let fabric = Array.init 10000 ~f:(fun _ -> Array.init 10000 ~f:(const 0)) in
  List.iter claims ~f:(fun {position; area; _;} -> 
    let (pos_x, pos_y) = position in
    let (area_x, area_y) = area in
    List.iter (List.range pos_x (pos_x + area_x)) ~f:(fun x -> 
      List.iter (List.range pos_y (pos_y + area_y)) ~f:(fun y -> 
        fabric.(x).(y) <- fabric.(x).(y) + 1
      )
    )
  );
  fabric


let%expect_test _ =  
  print_endline "Part 1";
  let fabric = partOne claims in
  let overClaimed = Array.fold fabric ~init:0 ~f:(fun total row ->
    Array.fold row ~init:total ~f:(fun rowTotal cell -> 
      if cell > 1 then rowTotal + 1 else rowTotal
    )
  )
  in
  overClaimed |> [%sexp_of: int] |> print_s;

  print_endline "Part Two";
  let remainingClaimOpt = List.find claims ~f:(fun {position; area;_} -> 
    let (pos_x, pos_y) = position in
    let (area_x, area_y) = area in
    List.for_all (List.range pos_x (pos_x + area_x)) ~f:(fun x -> 
      List.for_all (List.range pos_y (pos_y + area_y)) ~f:(fun y -> 
        fabric.(x).(y) = 1
      )
    )
  ) in
  remainingClaimOpt |> [%sexp_of: claim option] |> print_s;
  [%expect {|
    Part 1
    119551
    Part Two
    ((id 1124) (position (754 254)) (area (23 13))) |}]

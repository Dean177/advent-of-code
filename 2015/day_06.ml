open Core
open Owl
open Toolbox

type op = TurnOn | TurnOff | Toggle
type instruction = op * Point.t * Point.t

module Parser = struct
  open Angstrom
  open Toolbox.Parser  
  let make_instruction a b c = (a, b, c)
  let bound = Point.create <$> (integer <* char ',') <*> integer
  let op = 
    string "turn on " *> return TurnOn <|>
    string "turn off " *> return TurnOff <|>
    string "toggle " *> return Toggle

  let instruction = make_instruction <$> op <*> bound <*> (string " through " *> bound)

  let parse str = parse_string_exn (sep_by_newline instruction) str
end

let light_instructions =
  In_channel.read_all "./day_06.txt" |> Parser.parse

let apply lights (op_name, (r1, c1), (r2, c2)) = 
  let operation = match op_name with
    | TurnOn -> Mat.map (const 1.)
    | TurnOff -> Mat.map (const 0.)
    | Toggle -> Mat.map (function | 0. -> 1. | 1. -> 0. | _ -> 0.)
  in 
  Matrix.with_slice_mut (r1, c1) (r2, c2) lights ~f:operation

let%expect_test _ =
  let lights = Mat.zeros 1000 1000 in
  List.iter light_instructions ~f:(apply lights);
  Mat.sum lights
  |> Mat.to_array
  |> [%sexp_of : float array] |> print_s;
  [%expect {| (400410) |}]

let apply_two lights (op, (r1, c1), (r2, c2)) = 
  let operation = match op with
    | TurnOn -> Mat.map (fun x -> x +. 1.)
    | TurnOff -> Mat.map (fun x -> Float.(max 0. (x - 1.)))
    | Toggle -> Mat.map (fun x -> x +. 2.)
  in 
  Matrix.with_slice_mut (r1, c1) (r2, c2) lights ~f:operation

let%expect_test _ =
  let lights = Mat.zeros 1000 1000 in
  List.iter light_instructions ~f:(apply_two lights);
  Mat.sum lights
  |> Mat.to_array
  |> [%sexp_of : float array] |> print_s;
  [%expect {| (15343601) |}]

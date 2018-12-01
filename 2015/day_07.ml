open Core
open Stdint

module Uint16 = struct  
  include Uint16
  let t_of_sexp s = Int.t_of_sexp s |> Uint16.of_int 
  let sexp_of_t t = Uint16.to_int t |> Int.sexp_of_t    
  let lshift x y = shift_left y x
  let rshift x y = shift_right y x
end

type source = Signal of Uint16.t | Wire of string [@@deriving sexp, variants]

type unary_op = Uint16.t -> Uint16.t [@@deriving sexp]
type binary_op = Uint16.t -> Uint16.t -> Uint16.t [@@deriving sexp]

type component = Unary of source * unary_op | Binary of source * binary_op * source [@@deriving sexp, variants]

module Parser = struct
  open Angstrom
  open Toolbox.Parser
  let wire_name = take_while (fun chr -> chr <> ' ' && chr <> '\n')

  let source = (signal <$> uint16) <|> (wire <$> wire_name)
  let destination = (string " -> " *> wire_name) 

  let and_gate = binary <$> source <* string " AND " <*> return Uint16.logand <*> source 
  let or_gate = binary <$> source <* string " OR " <*> return Uint16.logor <*> source
  let lshift_gate = unary <$> source <* string " LSHIFT " <*> (Uint16.lshift <$> integer)
  let rshift_gate = unary <$> source <* string " RSHIFT " <*> (Uint16.rshift <$> integer)
  let not_gate = unary <$> string "NOT " *> source <*> (return Uint16.lognot)
  let wire = unary <$> source <*> return ident

  let component_input = whitespace *> (and_gate <|> or_gate <|> lshift_gate <|> rshift_gate <|> not_gate <|> wire)
  let component = 
    component_input >>= fun component_i ->
    destination >>= fun wire_n -> 
    return (wire_n, component_i)

  let parse str = parse_string_exn (sep_by_newline component) str |> String.Map.of_alist_exn
end

let value_of components wire_name = 
  let value_cache = Hashtbl.create (module String) in
  let rec source_value = function
    | Signal s -> s
    | Wire wire -> (
        match Hashtbl.find value_cache wire with
        | Some v -> v
        | None -> (
            let value = 
              match Map.find_exn components wire with
              | Unary (input, op) -> op (source_value input)
              | Binary (l, op, r) -> op (source_value l) (source_value r)
            in
            Hashtbl.set value_cache ~key:wire ~data:value;
            value
          )
      )
  in
  source_value (Wire wire_name)


let sample_components = 
  Parser.parse {|123 -> x
                 456 -> y
                 x AND y -> d
                 x OR y -> e
                 x LSHIFT 2 -> f
                 y RSHIFT 2 -> g
                 NOT x -> h
                 NOT y -> i|}

let%expect_test _ = 
  sample_components |> [%sexp_of : (component) String.Map.t] |> print_s;
  [%expect {|
      ((d (Binary (Wire x) <fun> (Wire y))) (e (Binary (Wire x) <fun> (Wire y)))
       (f (Unary (Wire x) <fun>)) (g (Unary (Wire y) <fun>))
       (h (Unary (Wire x) <fun>)) (i (Unary (Wire y) <fun>))
       (x (Unary (Signal 123) <fun>)) (y (Unary (Signal 456) <fun>))) |}]

let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "d") ~expect:(Uint16.of_int 72)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "e") ~expect:(Uint16.of_int 507)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "f") ~expect:(Uint16.of_int 492)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "g") ~expect:(Uint16.of_int 114)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "h") ~expect:(Uint16.of_int 65412)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "i") ~expect:(Uint16.of_int 65079)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "x") ~expect:(Uint16.of_int 123)
let%test_unit _ = [%test_result : Uint16.t] (value_of sample_components "y") ~expect:(Uint16.of_int 456)

let puzzle_components =
  In_channel.read_all "./day_07.txt" |> Parser.parse

let%expect_test _ = 
  value_of puzzle_components "a" |> Uint16.sexp_of_t |> print_s;
  [%expect {| 16076 |}]

let%expect_test _ = 
  let overridden_components = Map.set puzzle_components ~key:"b" ~data:(Unary (Signal (Uint16.of_int 16076), ident)) in  
  value_of overridden_components "a" |> Uint16.sexp_of_t |> print_s;
  [%expect {| 2797 |}]

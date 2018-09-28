open Angstrom
open! Core

type instruction = U | L | D | R [@@deriving sexp]

let parse_instruction = 
  (char 'U' *> return U) <|>
  (char 'L' *> return L) <|>
  (char 'D' *> return D) <|>
  (char 'R' *> return R)

let parse_instructions = sep_by (char '\n') (many parse_instruction) 

let%expect_test _ =
  parse_string parse_instructions "UUU\nDDD" |> unwrap |> [%sexp_of : ((instruction list) list)] |> Sexp.to_string |> print_endline;
  [%expect {| ((U U U)(D D D)) |}]

let grid_keypad start_key instruction = match start_key with 
  | 1 -> (match instruction with U -> 1 | L -> 1 | D -> 4 | R -> 2)
  | 2 -> (match instruction with U -> 2 | L -> 1 | D -> 5 | R -> 3)
  | 3 -> (match instruction with U -> 3 | L -> 2 | D -> 6 | R -> 3)
  | 4 -> (match instruction with U -> 1 | L -> 4 | D -> 7 | R -> 5)
  | 5 -> (match instruction with U -> 2 | L -> 4 | D -> 8 | R -> 6)
  | 6 -> (match instruction with U -> 3 | L -> 5 | D -> 9 | R -> 6)
  | 7 -> (match instruction with U -> 4 | L -> 7 | D -> 7 | R -> 8)
  | 8 -> (match instruction with U -> 5 | L -> 7 | D -> 8 | R -> 9)
  | 9 -> (match instruction with U -> 6 | L -> 8 | D -> 9 | R -> 9)
  | _ -> failwith ""

let solve_row keypad (start_value: 'a) (instructions : instruction list) : 'a = 
  List.fold_left ~init:start_value ~f:keypad instructions

let solve_keypad ~keypad ~start_value (instructions : (instruction list) list) : 'a list = 
  let (results, _) = List.fold_left 
      ~init:([], start_value) 
      ~f:(fun (ys, start_value) instruction_row -> 
          let y = solve_row keypad start_value instruction_row in 
          (y :: ys, y))
      instructions
  in
  List.rev results

let solve_grid (instructions : (instruction list) list) : int list = 
  solve_keypad ~keypad:grid_keypad ~start_value:5 instructions

let sample_instructions = [
  [U;L;L];
  [R;R;D;D;D];
  [L;U;R;D;L];
  [U;U;U;U;D]
] 

let%expect_test _ =
  let () = solve_grid sample_instructions 
           |> [%sexp_of : (int list)] |> Sexp.to_string_hum |> print_endline in
  [%expect {| (1 9 8 5) |}]

let%expect_test _ =
  In_channel.read_all "./code.txt" |> parse_string parse_instructions |> Result.ok_or_failwith |> solve_grid 
  |> [%sexp_of : (int list)] |> Sexp.to_string_hum |> print_endline;
  [%expect {| (1 9 6 3 6) |}]

let diamond_keypad start_key instruction = match start_key with 
  | '1' -> (match instruction with U -> '1' | L -> '1' | D -> '3' | R -> '1')
  | '2' -> (match instruction with U -> '2' | L -> '2' | D -> '6' | R -> '3')
  | '3' -> (match instruction with U -> '1' | L -> '2' | D -> '7' | R -> '4')
  | '4' -> (match instruction with U -> '4' | L -> '3' | D -> '8' | R -> '4')
  | '5' -> (match instruction with U -> '5' | L -> '5' | D -> '5' | R -> '6')
  | '6' -> (match instruction with U -> '2' | L -> '5' | D -> 'A' | R -> '7')
  | '7' -> (match instruction with U -> '3' | L -> '6' | D -> 'B' | R -> '8')
  | '8' -> (match instruction with U -> '4' | L -> '7' | D -> 'C' | R -> '9')
  | '9' -> (match instruction with U -> '9' | L -> '8' | D -> '9' | R -> '9')
  | 'A' -> (match instruction with U -> '6' | L -> 'A' | D -> 'A' | R -> 'B')
  | 'B' -> (match instruction with U -> '7' | L -> 'A' | D -> 'D' | R -> 'C')
  | 'C' -> (match instruction with U -> '8' | L -> 'B' | D -> 'C' | R -> 'C')
  | 'D' -> (match instruction with U -> 'B' | L -> 'D' | D -> 'D' | R -> 'D')
  | chr -> failwith ("Unrecognized char: " ^ Char.to_string chr)

let solve_diamond (instructions : (instruction list) list) : char list =   
  solve_keypad ~keypad:diamond_keypad ~start_value:'5' instructions

let%expect_test _ =
  solve_diamond sample_instructions 
  |> [%sexp_of : (char list)] |> Sexp.to_string_hum |> print_endline;
  [%expect {| (5 D B 3) |}]

let%expect_test _ =
  In_channel.read_all "./code.txt" |> parse_string parse_instructions |> Result.ok_or_failwith |> solve_diamond 
  |> [%sexp_of : (char list)] |> print_s;
  [%expect {| (3 C C 4 3) |}]
open Core

module Register = struct
  module T = struct 
    type t = A | B | C | D [@@ deriving compare, sexp]
  end
  include T
  include Comparator.Make(T)
  module Map = Map.Make(T)
end

type source = Value of int | Reg of Register.t [@@deriving sexp, variants]

type instruction =
  | Cpy of source * Register.t
  | Inc of Register.t
  | Dec of Register.t
  | Jnz of source * int
[@@deriving sexp, variants]

type state = {
  instruction_index : int;
  registers : int Register.Map.t; 
}

let value_at_register reg registers = Map.find_exn registers reg

let value_from_source registers = function
  | Value v -> v
  | Reg r -> value_at_register r registers

let rec process_i instructions {instruction_index; registers;} = 
  if instruction_index > (Array.length instructions - 1) then registers 
  else
    let next_instruction = Array.get instructions instruction_index in
    let new_state = match next_instruction with
      | Cpy (source, destination) -> { 
          instruction_index = instruction_index + 1; 
          registers = (Map.update registers destination ~f:(function | None -> 0 | Some _ -> value_from_source registers source));
        }
      | Inc register -> { 
          instruction_index = instruction_index + 1; 
          registers = (Map.update registers register ~f:(function | None -> 0 | Some reg_val -> reg_val + 1));
        }
      | Dec register -> { 
          instruction_index = instruction_index + 1; 
          registers = Map.update registers register ~f:(function | None -> 0 | Some reg_val -> reg_val - 1);
        }
      | Jnz (source, destin) -> 
        if value_from_source registers source <> 0
        then { instruction_index = instruction_index + destin; registers}
        else { instruction_index = instruction_index + 1; registers}
    in
    process_i instructions new_state

module Parser = struct
  open Angstrom

  let unwrap_exn = function 
    | Ok r -> r
    | Error err -> failwith err

  let integer = take_while1 (function | '-' | '0' .. '9' -> true | _ -> false) >>| Int.of_string
  let space = char ' '
  let label lbl = string lbl <* space
  let reigster = 
    (char 'a' *> return Register.A) <|>
    (char 'b' *> return Register.B) <|>
    (char 'c' *> return Register.C) <|>
    (char 'd' *> return Register.D)

  let source = (value <$> integer) <|> (reg <$> reigster)
  let instruction = 
    (label "cpy" *> (cpy <$> source <*> (space *> reigster))) <|>
    (label "inc" *> (inc <$> reigster)) <|>
    (label "dec" *> (dec <$> reigster)) <|>
    (label "jnz" *> (jnz <$> source <*> (space *> integer)))

  let instructions_parser = sep_by (char '\n') (many space *> instruction)

  let raw_instructions = {|cpy 1 a
                           cpy 1 b
                           cpy 26 d
                           jnz c 2
                           jnz 1 5
                           cpy 7 c
                           inc d
                           dec c
                           jnz c -2
                           cpy a c
                           inc a
                           dec b
                           jnz b -2
                           cpy c b
                           dec d
                           jnz d -6
                           cpy 17 c
                           cpy 18 d
                           inc a
                           dec d
                           jnz d -2
                           dec c
                           jnz c -5
                         |}

  let parsed_instructions = parse_string instructions_parser raw_instructions |> unwrap_exn |> (Array.of_list)

  let test_instructionms = 
    let test_input = {|cpy 41 a
    inc a
    inc a
    dec a
    jnz a 2
    dec a|}
    in
    parse_string instructions_parser test_input |> unwrap_exn |> (Array.of_list)

  let%expect_test _ = 
    parsed_instructions  |> [%sexp_of : instruction array] |> Sexp.to_string_hum |> print_endline;
    [%expect {|
      ((Cpy (Value 1) A) (Cpy (Value 1) B) (Cpy (Value 26) D) (Jnz (Reg C) 2)
       (Jnz (Value 1) 5) (Cpy (Value 7) C) (Inc D) (Dec C) (Jnz (Reg C) -2)
       (Cpy (Reg A) C) (Inc A) (Dec B) (Jnz (Reg B) -2) (Cpy (Reg C) B) (Dec D)
       (Jnz (Reg D) -6) (Cpy (Value 17) C) (Cpy (Value 18) D) (Inc A) (Dec D)
       (Jnz (Reg D) -2) (Dec C) (Jnz (Reg C) -5)) |}]
end

let inital_state = {
  instruction_index = 0;
  registers = (Register.Map.of_alist_exn [(A, 0);( B, 0); (C, 0); (D, 0)]);
}

let%expect_test _ = 
  process_i Parser.parsed_instructions inital_state |> [%sexp_of : int Register.Map.t] |> Sexp.to_string_hum |> print_endline;
  [%expect {| ((A 318117) (B 196418) (C 0) (D 0)) |}]

let part_two_inital_state = {
  instruction_index = 0;
  registers = (Register.Map.of_alist_exn [(A, 0);( B, 0); (C, 1); (D, 0)]);
}

let%expect_test _ = 
  process_i Parser.parsed_instructions part_two_inital_state |> [%sexp_of : int Register.Map.t] |> Sexp.to_string_hum |> print_endline;
  [%expect {| ((A 9227771) (B 5702887) (C 0) (D 0)) |}]
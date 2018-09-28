open Core

let puzzle_input = "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

module Tile = struct
  type t = Safe | Trap [@@deriving sexp]

  let of_char = function  
    | '^' -> Trap
    | '.' -> Safe
    | x -> failwith ("Unrecognized tile" ^ Char.to_string x)  

  let to_char = function
    | Trap -> '^' 
    | Safe -> '.' 

  let is_safe = function
    | (Trap, Trap, Safe) -> Trap
    | (Safe, Trap, Trap) -> Trap
    | (Trap, Safe, Safe) -> Trap
    | (Safe, Safe, Trap) -> Trap
    | _ -> Safe
end

module Parser = struct
  let parse str = String.to_array str |> Array.map ~f:Tile.of_char
end

let get_safe row i = 
  if i < 0 || i >= Array.length row 
  then Tile.Safe 
  else Array.get row i

let gen_next_row row = 
  Array.mapi row ~f:(fun i tile -> 
      Tile.is_safe (get_safe row (i - 1), tile, get_safe row (i + 1)))

let gen_rows n seed =
  let rec loop n acc = match n, acc with 
    | 0, _ -> acc
    | _, [] -> failwith "lol"
    | _, rw :: rws -> loop (n - 1) (gen_next_row rw :: rw :: rws)
  in 
  loop n [seed]

let%expect_test "d" =
  Parser.parse ".^^.^.^^^^" 
  |> gen_rows 10
  |> List.map ~f:(fun row -> 
      Array.map row ~f:Tile.to_char
      |> Array.to_list
      |> String.of_char_list
    )
  |> [%sexp_of : string list]
  |> print_s;
  [%expect {|
    (^^.^.^^^^^ ^^.^^^..^^ .^^^..^.^^ ^..^^^^.^^ ^^^^..^^^. ^^..^.^^.. .^^^^.^^.^
     ..^^...^^^ ^.^^.^.^^. ^^^...^..^ .^^.^.^^^^) |}]

let%expect_test "d" =
  Parser.parse puzzle_input
  |> gen_rows 399999
  |> List.bind ~f:(fun row -> Array.filter row ~f:((=) Tile.Safe) |> Array.to_list)
  |> List.length
  |> [%sexp_of : int]
  |> print_s;
  [%expect {|
    20003246 |}]
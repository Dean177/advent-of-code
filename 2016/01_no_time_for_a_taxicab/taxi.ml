open Angstrom
open Core

module Coordinates = struct
  module T = struct
    type t = { x : int; y : int; } [@@deriving compare, sexp]     

    let distance {x; y;} = (Int.abs x) + (Int.abs y)

    let%test _ =
      distance {x = -2; y = 3} = 5
  end
  include T
  include Comparator.Make(T)
end

type facing =  N | E | S | W [@@deriving sexp]

type state = {
  location: Coordinates.t;
  facing : facing;
} [@@deriving sexp]

type instruction = 
  | R of int
  | L of int
[@@deriving sexp]

let left i = L i
let right i = R i

let integer = Int.of_string <$> take_while1 (function '0'..'9' -> true | _ -> false)
let parse_left = left <$> (char 'L' *> integer)   
let parse_right = right <$> (char 'R' *> integer) 
let parse_instructions = sep_by1 (string ", ") (parse_left <|> parse_right)

let taxi_distance instruction = match instruction with R x -> x | L y -> y

let turn facing instruction = match instruction with 
  | R _ -> (match facing with N -> E | E -> S | S -> W | W -> N)
  | L _ ->  (match facing with N -> W | E -> N | S -> E | W -> S) 

let walk ({ x; y;} : Coordinates.t) facing n = 
  let open Coordinates in
  match facing with
  | N -> { x = x; y = y + n }
  | E -> { x = x + n ; y = y }
  | S -> { x = x; y = y - n }
  | W -> { x = x - n; y = y }

let step {facing;location} instruction = 
  let now_facing = turn facing instruction in
  { facing = now_facing; location = walk location now_facing (taxi_distance instruction) }

let initial_state = {
  location = { x = 0; y = 0 };
  facing = N;
}

let find_destination (state : state) (instructions : instruction list) = 
  List.fold_left 
    instructions
    ~init:state
    ~f:step

let solve_part_1 instructions =   
  Coordinates.distance (find_destination initial_state instructions).location

let%expect_test _ =
  let parsed_instructions = Result.ok_or_failwith (parse_string parse_instructions "R2, L3") in
  solve_part_1 parsed_instructions |> Int.to_string |> print_endline;
  [%expect {| 5 |}]

let%expect_test _ =
  let parsed_instructions = Result.ok_or_failwith (parse_string parse_instructions "R2, R2, R2") in
  solve_part_1 parsed_instructions |> Int.to_string |> print_endline;
  [%expect {| 2 |}]

let%expect_test _ =
  let parsed_instructions = Result.ok_or_failwith (parse_string parse_instructions "R5, L5, R5, R3") in
  solve_part_1 parsed_instructions |> Int.to_string |> print_endline;
  [%expect {| 12 |}]

let%expect_test _ =
  In_channel.read_all "./directions.txt" 
  |> parse_string parse_instructions |> Result.ok_or_failwith
  |> solve_part_1 
  |> Int.to_string |> print_endline;  
  [%expect {| 253 |}]
;;

let itermediate_coords ({x;y}:Coordinates.t) {location;facing} = 
  let open Coordinates in
  match facing with
  | N -> List.init (Int.abs (location.y - y)) ~f:(fun delta -> {x; y = y + delta})
  | E -> List.init (Int.abs (location.x - x)) ~f:(fun delta -> {x = x + delta; y}) 
  | S -> List.init (Int.abs (location.y - y)) ~f:(fun delta -> {x; y = y - delta}) 
  | W -> List.init (Int.abs (location.x - x)) ~f:(fun delta -> {x = x - delta; y})

let solve_part_2 instructions = 
  let rec find_duplicate_location accum state instructions = match instructions with 
    | [] -> None
    | instruction :: rest -> 
      let new_state = step state instruction in      
      let visited_coordinates = itermediate_coords state.location new_state in
      match List.find visited_coordinates ~f:(fun c -> Set.mem accum c) with 
      | Some duplicated_coords -> Some duplicated_coords
      | None -> find_duplicate_location (Set.union accum (Set.of_list (module Coordinates) visited_coordinates)) new_state rest
  in
  Option.map 
    ~f:Coordinates.distance 
    (find_duplicate_location (Set.empty (module Coordinates)) initial_state instructions)

let%expect_test _ =
  let parsed_instructions = Result.ok_or_failwith (parse_string parse_instructions "R8, R4, R4, R8") in
  solve_part_2 parsed_instructions |> [%sexp_of : int option] |> Sexp.to_string |> print_endline;
  [%expect {| (4) |}]

let%expect_test _ =
  In_channel.read_all "./directions.txt" 
  |> parse_string parse_instructions |> Result.ok_or_failwith 
  |> solve_part_2  
  |> [%sexp_of : int option] |> Sexp.to_string |> print_endline;
  [%expect {| (126) |}]

open Core

module Point = struct
  module T = struct
    type t = (int * int) [@@deriving compare, hash, sexp_of]
    let levenstein (a, b) (x, y) = Int.abs (x - a) + Int.abs (y - b)
  end
  include T
  include Comparator.Make(T)

  let equal a b = compare a b = 0
  let to_string (x, y) = String.concat ["("; Int.to_string x; ","; Int.to_string y;")"]  
end

let move (x, y) = function 
  | '>' -> x + 1, y
  | '<' -> x - 1, y
  | '^' -> x , y + 1
  | 'v' -> x, y - 1
  | chr -> failwith (String.of_char chr)

let puzzle_input = In_channel.read_all "./day_03.txt" 
let initial_children = Map.of_alist_exn (module Point) [((0,0), 1)]

let%expect_test _ =
  String.fold puzzle_input 
    ~init:(initial_children, (0, 0)) 
    ~f:(fun (visited_children, position) direction ->
        let new_position = move position direction in
        let new_children = (Map.update visited_children new_position ~f:(function | None -> 1 | Some x -> x + 1)) in 
        (new_children, new_position))
  |> fst
  |> Map.length
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 2572 |}]

type turn = Santa | RoboSanta
type state = {
  children : Set.M(Point).t;
  mover : turn;
  santa : int * int;
  robot : int * int;
}
let initial_state_year_two = {
  children = Set.singleton (module Point) (0,0);
  mover = Santa;
  santa = 0, 0;
  robot = 0, 0;
}

let%expect_test _ =
  String.fold puzzle_input 
    ~init:initial_state_year_two
    ~f:(fun {children; mover; santa; robot} direction ->
        let new_santa, new_robot = match (mover, santa, robot) with
          | Santa, s, r -> (move s direction, r)
          | RoboSanta, s, r -> (s, move r direction)
        in
        { 
          children = Set.add (Set.add children new_santa) new_robot;
          santa = new_santa;
          robot = new_robot;
          mover = match mover with 
            | Santa -> RoboSanta 
            | RoboSanta -> Santa
        }
      )
  |> (fun {children;_} -> Set.length children)
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 2631 |}]
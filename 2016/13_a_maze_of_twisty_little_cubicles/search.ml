open Core

let puzzle_input = 1358

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

let%test_unit _ = [%test_result : int] (Int.compare (Point.hash (7, 1)) (Point.hash (1, 7))) ~expect:1

let binary_representation n =
  if n < 0 then invalid_arg (Int.to_string n) else
  if n = 0 then "0" else
    let rec loop acc d =
      if d = 0 then acc 
      else loop (Int.to_string (d land 1) :: acc) (d lsr 1)
    in
    String.concat (loop [] n)

let%test_unit _ = [%test_result : string] (binary_representation 7) ~expect:"111"
let%test_unit _ = [%test_result : string] (binary_representation 33) ~expect:"100001"

let bits_set num = 
  String.fold (binary_representation num) ~init:0 ~f:(fun total chr -> 
      match chr with 
      | '0' -> total
      | '1' -> total + 1
      | _ -> failwith "Non-binary digit encountered"
    )

let%test_unit _ = [%test_result : int] (bits_set 0) ~expect:0
let%test_unit _ = [%test_result : int] (bits_set 7) ~expect:3
let%test_unit _ = [%test_result : int] (bits_set 33) ~expect:2

let is_open_space ?(seed = puzzle_input) (x, y) =
  let ones = 
    (x * x + 3 * x + 2 * x * y + y + y * y)
    |> ( + ) seed
    |> bits_set
  in  
  ones % 2 = 0

let%test_unit _ = [%test_result : bool] (is_open_space (0,0)) ~expect:true
let%test_unit _ = [%test_result : bool] (is_open_space ~seed:10 (0,9)) ~expect:false
let%test_unit _ = [%test_result : bool] (is_open_space ~seed:10 (2,5)) ~expect:false
let%test_unit _ = [%test_result : bool] (is_open_space ~seed:10 (4,2)) ~expect:true
let%expect_test _ =
  Array.init 7 ~f:(fun y -> Array.init 10 ~f:(fun x -> if is_open_space ~seed:10 (x, y) then '.' else '#'))
  |> [%sexp_of : char array array] |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    ((. # . # # # # . # #) (. . # . . # . . . #) (# . . . . # # . . .)
     (# # # . # . # # # .) (. # # . . # . . # .) (. . # # . . . . # .)
     (# . . . # # . # # #)) |}]

let surrounding_open_space ?(seed = puzzle_input) (x, y) = 
  [x + 1, y; x - 1, y; x, y + 1; x, y - 1] |> List.filter ~f:(is_open_space ~seed)

let%expect_test _ =
  surrounding_open_space ~seed:10 (1,1) |> [%sexp_of : Point.t list] |> Sexp.to_string_hum |> print_endline;
  [%expect {| ((0 1) (1 2)) |}]

let rec reconstruct_path ~acc cameFrom current  =
  match Hashtbl.find cameFrom current with
  | None -> acc
  | Some previous_step -> reconstruct_path cameFrom previous_step ~acc:(current :: acc)

let%expect_test _ =
  reconstruct_path 
    ~acc:[] 
    (Hashtbl.of_alist_exn (module Point) [((4, 4), (4, 3)); (4, 3), (4, 2); (4, 2), (4, 1)]) 
    (4, 4) 
  |> [%sexp_of : Point.t list] |> print_s;
  [%expect {| ((4 2) (4 3) (4 4)) |}]

let a_star ~seed start goal : Point.t list option =
  (* // The cost of getting from the start node to that node. *)   
  let gScore = Hashtbl.of_alist_exn (module Point) [start, 0]  in
  let lookup_gScore point = 
    Hashtbl.find gScore point |> Option.value ~default:Int.max_value
  in
  (* // The total cost of getting from the start node to the goal by passing by that node. *)  
  let fScore = Hashtbl.of_alist_exn (module Point) [start, Point.levenstein start goal] in
  let lookup_fscore point = 
    Hashtbl.find fScore point |> Option.value ~default:Int.max_value
  in

  (* For each node, which node it can most efficiently be reached from.
     If a node can be reached from many nodes, cameFrom will eventually contain the
     most efficient previous step. *)
  let cameFrom = Hashtbl.of_alist_exn (module Point) [] in

  let evaluated_nodes = Hash_set.of_list (module Point) [] in

  (* The prioritised heap of currently discovered nodes that have not been evaluated yet. *)
  let unevaluated_nodes = 
    Heap.create 
      ~min_size:(Point.levenstein start goal)
      ~cmp:(fun x y -> (lookup_fscore x) - (lookup_fscore y))
      ()
  in
  Heap.add unevaluated_nodes start;

  let rec loop iterations =    
    match Heap.pop unevaluated_nodes with
    | None -> None
    | Some current -> 
      if (Point.equal current goal) then      
        Some (reconstruct_path ~acc:[] cameFrom current)
      else (
        Hash_set.add evaluated_nodes current;
        List.filter (surrounding_open_space ~seed current) ~f:(fun neighbor -> not (Hash_set.mem evaluated_nodes neighbor))
        |> List.iter ~f:(fun neighbor -> 
            (* // The distance from start to a neighbor *)
            let tentative_gScore = lookup_gScore current + Point.levenstein current neighbor in                    
            Heap.add unevaluated_nodes neighbor;
            if (tentative_gScore < lookup_gScore neighbor) then ( 
              (* // This path is the best until now. Record it! *)
              Hashtbl.set cameFrom ~key:neighbor ~data:current;
              Hashtbl.set gScore ~key:neighbor ~data:tentative_gScore;
              Hashtbl.set fScore ~key:neighbor ~data:(tentative_gScore + (Point.levenstein neighbor goal));
            )
            else ()
          );
        loop (iterations + 1)
      )
  in
  loop 0

let%expect_test _ =
  a_star ~seed:10 (1,1) (7, 4)
  |> Option.value_exn
  |> List.length 
  |> Int.to_string 
  |> print_endline;
  [%expect {| 11 |}]

let%expect_test _ =
  a_star ~seed:puzzle_input (1,1) (31, 39)
  |> Option.value_exn
  |> List.length 
  |> Int.to_string 
  |> print_endline;
  [%expect {| 96 |}]

let potential_points : Point.t list array = Array.init 50 ~f:(fun i -> 
    let x = i + 1 in
    let y = (50 - x) + 1 in 
    match x, y with
    | 0, 0 -> [(x, y);]
    | 0, _ -> [(x, y); (x, -y);]
    | 50, _ -> [(x, y); (-x, y);]
    | _, _ -> [(x, y); (x, -y); (-x, y); (-x, -y)]
  )

let%expect_test _ = 
  Array.fold potential_points ~init:0 ~f:(fun total point_list ->
      let reachable_point_paths : Point.t list list = List.filter_map point_list ~f:(fun point -> a_star ~seed:puzzle_input (1, 1) point) in
      let reachable_points = List.length reachable_point_paths in
      total + reachable_points    
    )
  |> Int.to_string
  |> print_endline;
  [%expect {| 12 |}]

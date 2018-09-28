open Core

let md5 str = str |> Md5.digest_string  |> Md5.to_hex

type step = Up | Down | Left | Right [@@deriving sexp]

let step_to_char = function 
  | Up -> 'U' 
  | Down -> 'D' 
  | Left -> 'L' 
  | Right -> 'R'

let position_to_step = function 
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | 3 -> Right
  | n -> invalid_arg (Int.to_string n)

let is_open position = function 'b' .. 'f' -> Some position | _ -> None

let remove_walls position direction = match (direction, position) with
  | Up, (_, 3) 
  | Down, (_, 0) 
  | Left, (0, _)
  | Right, (3, _) -> false
  | _ -> true


type path = step list

let open_doors hash path position = 
  hash ^ (String.rev (String.of_char_list (List.map path ~f:step_to_char)))
  |> md5  
  |> (fun str -> String.slice str 0 4)
  |> String.to_list
  |> List.filter_mapi ~f:(fun i char -> is_open (position_to_step i) char)
  |> List.filter ~f:(remove_walls position)

let move (x, y) direction = match direction with
  | Up -> (x, y + 1)
  | Down -> (x, y - 1) 
  | Left ->  (x - 1, y)
  | Right -> (x + 1, y)

let%expect_test _ =
  open_doors "hijkl" [] (0, 3) |> [%sexp_of : step list] |> print_s;
  [%expect {| (Down) |}]

let%expect_test _ =
  open_doors "hijkl" [Down] (0, 2) |> [%sexp_of : step list] |> print_s;
  [%expect {| (Up Right) |}]

let%expect_test _ =
  open_doors "hijkl" (List.rev [Down; Right]) (1, 2) |> [%sexp_of : step list] |> print_s;
  [%expect {| () |}]

let%expect_test _ =
  open_doors "hijkl" (List.rev [Down; Up]) (0, 3) |> [%sexp_of : step list] |> print_s;
  [%expect {| (Right) |}]

let%expect_test _ =
  open_doors "hijkl" (List.rev [Down; Up; Right]) (1, 3) |> [%sexp_of : step list] |> print_s;
  [%expect {| () |}]

type reachable_position = {
  path: step list;
  position: int * int;
}

let search passcode start goal = 
  let search_states = Heap.create ~cmp:(fun state_a state_b -> Int.compare (List.length state_a.path)  (List.length state_b.path)) () in  
  let rec loop {position; path;} = 
    if position = goal then 
      List.rev path 
    else (
      open_doors passcode path position |> List.iter ~f:(fun direction -> 
          Heap.add search_states { position = (move position direction); path = (direction :: path); };
        );
      loop (Heap.pop_exn search_states);
    )
  in
  loop { position = start; path = [] }

let%expect_test _ =
  search "ihgpwlah" (0, 3) (3, 0) |> List.map ~f:step_to_char |> String.of_char_list |> print_endline;
  [%expect {| DDRRRD |}]

let%expect_test _ =
  search "kglvqrro" (0, 3) (3, 0) |> List.map ~f:step_to_char |> String.of_char_list |> print_endline;
  [%expect {| DDUDRLRRUDRD |}]

let%expect_test _ =
  search "ulqzkmiv" (0, 3) (3, 0) |> List.map ~f:step_to_char |> String.of_char_list |> print_endline;
  [%expect {| DRURDRUDDLLDLUURRDULRLDUUDDDRR |}]

let%expect_test _ =
  search "gdjjyniy" (0, 3) (3, 0) |> List.map ~f:step_to_char |> String.of_char_list |> print_endline;
  [%expect {| DUDDRLRRRD |}]

let longest_paths passcode start goal = 
  let search_states = Heap.create ~cmp:(fun state_a state_b -> Int.compare (List.length state_a.path)  (List.length state_b.path)) () in  
  let rec loop next_position discovered_paths =     
    match next_position with
    | None -> discovered_paths
    | Some {position; path} -> (
        if position = goal then 
          loop (Heap.pop search_states) (path :: discovered_paths) 
        else (
          open_doors passcode path position |> List.iter ~f:(fun direction -> 
              Heap.add search_states { position = (move position direction); path = (direction :: path); };
            );
          loop (Heap.pop search_states) (discovered_paths)
        )
      )
  in
  loop (Some { position = start; path = [] }) []

let%expect_test _ =
  longest_paths "kglvqrro" (0, 3) (3, 0) 
  |> List.max_elt ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
  |> Option.value_exn
  |> List.length |> Int.to_string |> print_endline;
  [%expect {| 492 |}]

let%expect_test _ =
  longest_paths "ulqzkmiv" (0, 3) (3, 0) 
  |> List.max_elt ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
  |> Option.value_exn
  |> List.length |> Int.to_string |> print_endline;
  [%expect {| 830 |}]

let%expect_test _ =
  longest_paths "gdjjyniy" (0, 3) (3, 0) 
  |> List.max_elt ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
  |> Option.value_exn
  |> List.length |> Int.to_string |> print_endline;
  [%expect {| 578 |}]
open Core

let rec contains_tripple = function
  | [] | [_] | [_;_]  -> None
  | a :: b :: c :: xs -> 
    if a = b && a = c then Some a else contains_tripple xs

let rec contains_pentiple = function
  | [] | [_] | [_;_] | [_;_;_] | [_;_;_;_] -> None
  | a :: b :: c :: d :: e :: xs -> 
    if a = b && b = c && c = d && d = e then Some a else contains_pentiple xs

let md5 str = str |> Md5.digest_string  |> Md5.to_hex

type potential_key = {
  key_text : string;
  tripple_char : char;
}

type actual_key = {
  key_text : string;
  index : int;
}

type state = {
  index : int;
  keys : actual_key list;
  potential_keys : potential_key Queue.t;
}
let rec solve_1 salt state =
  if List.length state.keys = 64 then 
    state.keys 
  else (
    ignore (Queue.dequeue state.potential_keys);    
    let new_key = md5 (salt ^ Int.to_string state.index) in
    let () = match (contains_tripple (String.to_list new_key)) with
      | None -> ()
      | Some chr -> Queue.enqueue state.potential_keys {key_text = new_key; tripple_char = chr}
    in
    let new_keys = match contains_pentiple (String.to_list new_key) with
      | None -> state.keys
      | Some cccchr -> (
          match Queue.find state.potential_keys ~f:(fun {tripple_char; _} -> tripple_char = cccchr) with
          | Some _ -> { key_text = new_key; index = state.index } :: state.keys
          | None -> state.keys
        )
    in

    solve_1 salt { 
      state with 
      index = state.index + 1; 
      keys = new_keys;
    }
  )

let%expect_test _ =
  let initial_state = {
    index = 0; 
    keys = []; 
    potential_keys = (Queue.init 1000 ~f:(fun _ -> {key_text = ""; tripple_char = '\n';})) 
  } in
  solve_1 "abc" initial_state
  |> List.hd_exn 
  |> fun ({index;_} : actual_key) -> index 
  |> Int.to_string
  |> print_endline;
  [%expect {| 596713 |}]

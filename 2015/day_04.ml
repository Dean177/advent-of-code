open Core

let rec find_hash puzzle_input int = 
  let hash = Md5.digest_string (puzzle_input ^ Int.to_string int) |> Md5.to_hex in
  if (String.for_all (String.slice hash 0 5) ~f:((=) '0')) then
    int 
  else 
    find_hash puzzle_input (int + 1)

let%expect_test _ =
  find_hash "ckczppom" 0 |> Int.to_string |>  print_endline;
  [%expect {| 117946 |}]


let rec find_deeper_hash puzzle_input int = 
  let hash = Md5.digest_string (puzzle_input ^ Int.to_string int) |> Md5.to_hex in
  if (String.for_all (String.slice hash 0 6) ~f:((=) '0')) then
    (int, hash) 
  else 
    find_deeper_hash puzzle_input (int + 1)

let%expect_test _ =
  find_deeper_hash "ckczppom" 0 |> [%sexp_of : (int * string)] |>  print_s;
  [%expect {| (3938038 00000028023e3b4729684757f8dc3fbf) |}]
open Core

let hex_md5 door_id index = Md5.digest_string (door_id ^ Int.to_string index) |> Md5.to_hex
let%test_unit _ = [%test_result : string] (hex_md5 "abc" 3231929) ~expect:"00000155f8105dff7f56ee10fa9b9abd"
let is_interesting_hash = String.is_prefix ~prefix:"00000"
(* let password_character_of_hex md5_hex = String.get md5_hex 5
let password_character md5_hex = 
  if is_interesting_hash md5_hex 
  then Some (password_character_of_hex md5_hex) 
  else None *)

(* let find_door_passcode door_id = 
   let rec find_next_character index = match password_character (hex_md5 door_id index) with 
    | Some c -> (c, index)
    | None -> find_next_character (index + 1)
   in

   let rec find_password found_characters index =
    if List.length found_characters = 8 then String.of_char_list (List.rev found_characters) 
    else (
      let (chr, new_idx) = find_next_character index in      
      find_password (chr :: found_characters) (new_idx + 1)
    )
   in
   find_password [] 0 *)

(* let%expect_test _ =
   find_door_passcode "ugkcyxxp" |>  print_endline;
   [%expect {||}] *)

type pass_char = { position: int; char: char } [@@deriving sexp]

let password_character_with_position md5_hex = 
  let position = Char.to_int (String.get md5_hex 5) in
  let char = String.get md5_hex 6 in
  let is_valid_position = position >= 0 && position < 8 in
  if is_interesting_hash md5_hex && is_valid_position
  then Some { position; char }
  else None

let find_enhanced_passcode door_id = 
  let password = Array.init 8 ~f:(const '-') in
  let rec find_next_character index = match password_character_with_position (hex_md5 door_id index) with 
    | Some pass_char -> (pass_char, index)
    | None -> find_next_character (index + 1)
  in

  let rec find_password found_characters index =
    if found_characters = 8 || index > 10000 then found_characters
    else (
      let ({char; position}, new_idx) = find_next_character index in      
      let () = print_endline (password |> [%sexp_of : char array] |> Sexp.to_string) in
      let () = Array.set password position char in
      find_password (found_characters + 1) (new_idx + 1)
    )
  in
  let _ = find_password 0 0 in
  password

let%expect_test _ =
  find_enhanced_passcode "ugkcyxxp" |> [%sexp_of : char array] |> Sexp.to_string |> print_endline;
  [%expect {||}]
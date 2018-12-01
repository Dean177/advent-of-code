open Core

type room = {
  checksum: string;
  encrypted_name: string;
  sector_id: int;
} [@@deriving sexp]

let make_room encrypted_name sector_id checksum = {encrypted_name;sector_id;checksum}

module Parser = struct
  open Angstrom
  let unwrap = function 
    | Ok a -> a
    | Error str -> failwith str

  let lowercase_characters = take_while1 (function 'a' .. 'z' -> true | _ -> false)  
  let sector_id = char '-' *> take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string
  let encrypted_name  = String.concat <$> (sep_by (char '-') lowercase_characters)
  let checksum = (char '[' *> lowercase_characters <* char ']')
  let room = make_room <$> encrypted_name <*> (sector_id) <*> checksum
  let parse_room str = parse_string room str |> Result.ok_or_failwith

  let%expect_test _ =
    let () = (parse_room "aaaaa-bbb-z-y-x-123[abxyz]" 
              |> [%sexp_of : room] |> Sexp.to_string_hum |> print_endline) in
    [%expect {| ((checksum abxyz) (encrypted_name aaaaabbbzyx) (sector_id 123)) |}]

  let rooms = sep_by (char '\n') room
  let parse str = parse_string rooms str |> Result.ok_or_failwith

  let%expect_test _ =
    let test_rows = {eof|vxupkizork-sgmtkzoi-pkrrehkgt-zxgototm-644[kotgr]
                         mbiyqoxsm-pvygob-nocsqx-900[obmqs]|eof} in
    let () = parse test_rows
             |> [%sexp_of : room list] |> Sexp.to_string_hum 
             |> print_endline 
    in
    [%expect {|
      (((checksum kotgr) (encrypted_name vxupkizorksgmtkzoipkrrehkgtzxgototm)
        (sector_id 644))) |}]
end

let compare_char_counts (char_a, count_a) (char_b, count_b) = match Int.compare count_b count_a with 
  | 0 -> Char.compare char_a char_b
  | n -> n

let char_counts (encrypted_name : string) = 
  String.to_list encrypted_name |> List.fold ~init:(Map.empty (module Char)) ~f:(fun char_counts char -> 
      match Map.find char_counts char with
      | None -> Map.set char_counts ~key:char ~data:1
      | Some total -> Map.set char_counts ~key:char ~data:(total + 1))

let%expect_test _ =   
  Map.to_alist (char_counts "aaaaabbbzyx") 
  |> List.sort ~compare:compare_char_counts
  |> [%sexp_of : (char * int) list] |> print_s;
  [%expect {| ((a 5) (b 3) (x 1) (y 1) (z 1)) |}]

let calc_checksum encrypted_name =     
  let sorted_character_count_tuples = Map.to_alist (char_counts encrypted_name) |> List.sort ~compare:compare_char_counts in   
  List.take sorted_character_count_tuples 5 
  |> List.map ~f:fst  
  |> String.of_char_list

let%expect_test _ = 
  calc_checksum "aaaaabbbzyx" |> print_endline;
  [%expect {| abxyz |}]

let is_real { encrypted_name; checksum; _ } = 
  String.equal (calc_checksum encrypted_name) checksum 

let%test _ = is_real (Parser.parse_room "aaaaa-bbb-z-y-x-123[abxyz]")
let%test _ = is_real (Parser.parse_room "a-b-c-d-e-f-g-h-987[abcde]")
let%test _ = is_real (Parser.parse_room "not-a-real-room-404[oarel]")
let%test _ = not (is_real (Parser.parse_room "totally-real-room-200[decoy]"))

let solution rooms = 
  List.fold_left 
    rooms
    ~init:0
    ~f:(fun total room -> if is_real room then (total + room.sector_id) else total)

let%expect_test _ =
  In_channel.read_all "./rooms.txt" |> Parser.parse 
  |> solution 
  |> Int.to_string |> print_endline;
  [%expect {| 137896 |}]

let rotate positions chr = 
  (((Char.to_int chr - 97) + positions) % 26) + 97 |> Char.of_int_exn

let%test_unit _ = 
  [%test_result : int] (((Char.to_int 'a' - 97) + 13) % 26) ~expect:13

let%test_unit _ = 
  [%test_result : char] (rotate 1 'z') ~expect:'a'

let%expect_test _ =
  In_channel.read_all "./rooms.txt" |> Parser.parse 
  |> List.filter ~f:is_real 
  |> List.map ~f:(fun {encrypted_name; sector_id;_} -> (String.map ~f:(rotate sector_id) encrypted_name, sector_id) )
  |> List.find ~f:(fun (unencrypted_name, _) -> String.is_substring ~substring:"northpoleobjectstorage" unencrypted_name)
  |> [%sexp_of : (string * int) option] |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((northpoleobjectstorage 501)) |}]
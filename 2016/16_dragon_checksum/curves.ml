open Core

let dragon_round a = 
  let b = String.map (String.rev a) ~f:(function 
      | '0' -> '1'
      | '1' -> '0'
      | _ -> failwith "Unexpected character")
  in 
  a ^ "0" ^ b

let%test_unit _ = [%test_result : string] (dragon_round "1") ~expect:"100"
let%test_unit _ = [%test_result : string] (dragon_round "0") ~expect:"001"
let%test_unit _ = [%test_result : string] (dragon_round "11111") ~expect:"11111000000"
let%test_unit _ = [%test_result : string] (dragon_round "111100001010") ~expect:"1111000010100101011110000"

let rec dragon length a =
  if String.length a >= length 
  then String.slice a 0 length
  else (dragon length (dragon_round a))

let%test_unit _ = [%test_result : string] (dragon 20 "10000") ~expect:"10000011110010000111"

let to_pairs str = 
  let (paired_characters, _) = String.fold str ~init:([], None) ~f:(fun (seq, prev) char -> 
      match prev with 
      | None -> (seq, Some char)
      | Some a -> ((a, char) :: seq), None) 
  in
  List.map paired_characters ~f:(fun (l,r) -> if l = r then '1' else '0')
  |> String.of_char_list 
  |> String.rev

let rec checksum str = 
  let pairs = to_pairs str in
  if String.length pairs % 2 <> 0 
  then pairs 
  else checksum pairs

let%test_unit _ = [%test_result : string] (checksum "110010110100") ~expect:"100"

let%test_unit _ = [%test_result : string] (dragon 20 "10000" |> checksum) ~expect:"01100"

let%expect_test _ =
  let initial_state = "10001110011110000" in
  dragon 272 initial_state |> checksum |> print_endline;
  [%expect {| 10010101010011101 |}]

let%expect_test _ =
  let initial_state = "10001110011110000" in
  dragon 35651584 initial_state |> checksum |> print_endline;
  [%expect {| 01100111101101111 |}]
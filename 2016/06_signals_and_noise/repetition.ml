open Core

module Parser = struct  
  let parse = String.split ~on:'\n'
end

type count_by_character = (char, int, Char.comparator_witness) Map.t
let empty_character_count = Map.empty (module Char)

type t = (int, count_by_character, Int.comparator_witness) Map.t

let increment_character_count (position : int) (char_map : t) (character : char) : t =   
  Map.update char_map position ~f:(function
      | None -> (Char.Map.singleton character 1)
      | Some counts_for_chars -> (Map.update counts_for_chars character ~f:(function
          | None -> 1
          | Some count -> count + 1))
    )

let most_requent_character (char_to_int : count_by_character) : char =  
  let some_char_with_count = 
    char_to_int 
    |> Map.to_alist
    |> List.max_elt ~compare:(fun (_, count_a) (_, count_b) -> Int.compare count_a count_b) 
  in
  match some_char_with_count with
  | Some (character, _) -> character
  | None -> ' '

let decode str_list =   
  let character_counts : t = List.fold 
      str_list 
      ~init:(Int.Map.empty : t)
      ~f:(fun char_map message -> 
          String.foldi message 
            ~init:char_map 
            ~f:increment_character_count)
  in  
  Map.map character_counts ~f:most_requent_character 
  |> Map.to_alist ?key_order:(Some `Increasing) 
  |> List.map ~f:(fun (_, character) -> character) 
  |> String.of_char_list


let%expect_test _ =
  let test_str = 
    {eof|
      eedadn
      drvtee
      eandsr
      raavrd
      atevrs
      tsrnev
      sdttsa
      rasrtv
      nssdts
      ntnada
      svetve
      tesnvt
      vntsnd
      vrdear
      dvrsen
      enarar|eof} 
  in
  Parser.parse test_str |> decode |> print_endline;
  [%expect {| easter |}]

let%expect_test _ = 
  In_channel.read_all "./noise.txt" |> Parser.parse |> decode |> print_endline;
  [%expect {| afwlyyyq |}]
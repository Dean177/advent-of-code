open Core

type marker = { seq_length : int; repeats : int; } [@@deriving compare, sexp]

let make_marker seq_length repeats = {seq_length; repeats}
let marker_to_string {seq_length; repeats} = "(" ^ Int.to_string seq_length ^ "x" ^ Int.to_string repeats ^ ")"

let repeat n str = Array.init n ~f:(const str) |> String.concat_array

let arr_singleton a = [|a|]

type token = Mark of marker | Data of string [@@deriving compare, sexp, variants]

let rec take_n_tokens n tokens = match (n, tokens) with
  | 0, _ -> []
  | _, [] -> []
  | (n, (((Data _) as data) :: rest)) -> data :: (take_n_tokens (n - 1) rest)
  | (n, (((Mark marker) as mark) :: rest)) -> mark :: (take_n_tokens (n - String.length (marker_to_string marker)) rest)

let rec drop_n_tokens n tokens = match (n, tokens) with
  | 0, _ -> tokens  
  | _, [] -> []
  | (n, ((Data _) :: rest)) -> drop_n_tokens (n - 1) rest
  | (n, ((Mark marker) :: rest)) -> drop_n_tokens (n - String.length (marker_to_string marker)) rest

let rec decompressed_length tokens = 
  match tokens with    
  | [] -> 0
  | (Data _) :: rest -> 1 + (decompressed_length rest)
  | (Mark {seq_length; repeats}) :: rest -> (
      let repeat_section = take_n_tokens seq_length rest in
      (* [%sexp_of : token list] repeat_section |> print_s; *)
      let decompressed_section_length = decompressed_length repeat_section in         
      ((decompressed_section_length * repeats) + (decompressed_length (drop_n_tokens seq_length rest)))
    )  

module Parser = struct
  open Angstrom
  let characters : string t = take 1
  let integer : int t = take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string
  let parens a = (char '(' *> a) <* char ')'
  let marker : marker t = parens (make_marker <$> integer <*> char 'x' *> integer) 
  let marker_parser : string t = marker >>= (fun {seq_length; repeats} -> repeat repeats <$> take seq_length)

  let data_decompressor = many (marker_parser <|> characters)

  let parse str =
    parse_string data_decompressor str 
    |> Result.ok_or_failwith 
    |> String.concat

  let%test_unit _ = [%test_result : string] (parse "A(1x5)BC") ~expect:"ABBBBBC"
  let%test_unit _ = [%test_result : string] (parse "(3x3)XYZ") ~expect:"XYZXYZXYZ"
  let%test_unit _ = [%test_result : string] (parse "A(2x2)BCD(2x2)EFG") ~expect:"ABCBCDEFEFG"
  let%test_unit _ = [%test_result : string] (parse "(6x1)(1x3)A") ~expect:"(1x3)A"
  let%test_unit _ = [%test_result : string] (parse "X(8x2)(3x3)ABCY") ~expect:"X(3x3)ABC(3x3)ABCY"

  let token_parser = many ((mark <$> marker) <|> (data <$> characters))
  let parse_tokens str = parse_string token_parser str |> Result.ok_or_failwith 

  let%test_unit _ = [%test_result : token list] (parse_tokens "A(1x5)BC") ~expect:[Data "A";Mark {seq_length=1;repeats=5};Data "B";Data "C"]
  let%test_unit _ = [%test_result : int] (parse_tokens "A(1x5)BC" |> decompressed_length) ~expect:7
  let%test_unit _ = [%test_result : int] (parse_tokens "(3x3)XYZ" |> decompressed_length) ~expect:9
  let%test_unit _ = [%test_result : int] (parse_tokens "X(3x3)ABC(3x3)ABCY" |> decompressed_length) ~expect:(String.length "XABCABCABCABCABCABCY")
  let%test_unit _ = [%test_result : int] (parse_tokens "X(8x2)(3x3)ABCY" |> decompressed_length) ~expect:(String.length "XABCABCABCABCABCABCY")
  let%test_unit _ = [%test_result : int] (parse_tokens "(27x12)(20x12)(13x14)(7x10)(1x12)A" |> decompressed_length) ~expect:241920
end

let%expect_test _ =
  In_channel.read_all "./compressed.txt" |> Parser.parse |> String.length |> Int.to_string |> print_endline;
  [%expect {| 183269 |}]

let%expect_test _ =
  In_channel.read_all "./compressed.txt" |> Parser.parse_tokens |> decompressed_length |> Int.to_string |> print_endline;
  [%expect {| 11317278863 |}]

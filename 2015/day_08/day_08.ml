open Core

let puzzle_input = 
  In_channel.read_all "./day_08.txt" 
  |> String.split_lines

type token = 
  | Character of string
  | Backslash
  | EscapedQuotation
  | Hex of char * char
[@@deriving sexp]

module Parser = struct
  open Angstrom
  open Toolbox.Parser

  let backslash = char '\\' *> char '\\' *> return Backslash
  let escapedQuote = char '\\' *> char '"' *> return EscapedQuotation
  let hexadecimal = char '\\' *> char 'x' *> (
      hexCharacter >>= fun h1 -> 
      hexCharacter >>= fun h2 -> 
      return (Hex (h1, h2))
    )
  let singleCharacter = take 1 >>= fun c -> return (Character c)

  let token = backslash <|> escapedQuote <|> hexadecimal <|> singleCharacter

  let parseTokens = parse_string_exn (char '"' *> many_till (token) (char '"'))
end
let memoryLength str = List.length (Parser.parseTokens str)

let%test_unit _ = [%test_result : int] (memoryLength {|""|}) ~expect:0
let%test_unit _ = [%test_result : int] (memoryLength {|"abc"|}) ~expect:3
let%test_unit _ = [%test_result : int] (memoryLength {|"\x27"|}) ~expect:1
let%test_unit _ = [%test_result : int] (memoryLength {|"aaa\"aaa"|}) ~expect:7
let%test_unit _ = [%test_result : int] (memoryLength {|"\x83qg\"nwgugfmfpzlrvty\"ryoxm"|}) ~expect:25

let partOne input = 
  List.map input ~f:(fun str ->  String.length str - memoryLength str)
  |> List.fold ~init:0 ~f:( + )

let%test_unit _ = [%test_result : int] (partOne [{|""|}; {|"abc"|}; {|"aaa\"aaa"|};{|"\x27"|}]) ~expect:12

let encode tokens = 
  let tokenStrings = List.map tokens ~f:(function
      | Character a ->  a
      | Backslash ->  {|\\|}
      | EscapedQuotation -> {|\\\"|}
      | Hex (c1, c2) -> String.concat [{|\\x|}; Char.to_string c1; Char.to_string c2]
    ) in
  String.concat ([{|"\"|}] @ tokenStrings @ [{|\""|}])

let%test_unit _ = [%test_result : string] (Parser.parseTokens {|""|} |> encode) ~expect:{|"\"\""|}
let%test_unit _ = [%test_result : int] (Parser.parseTokens {|""|} |> encode |> String.length) ~expect:6
let%test_unit _ = [%test_result : string] (Parser.parseTokens {|"abc"|} |> encode) ~expect:{|"\"abc\""|}
let%test_unit _ = [%test_result : string] (Parser.parseTokens {|"aaa\"aaa"|} |> encode) ~expect:{|"\"aaa\\\"aaa\""|}
let%test_unit _ = [%test_result : string] (Parser.parseTokens {|"\x27"|} |> encode) ~expect:{|"\"\\x27\""|}
let%test_unit _ = [%test_result : string] (Parser.parseTokens {|"\xff"|} |> encode) ~expect:{|"\"\\xff\""|}
let%test_unit _ = [%test_result : string] (Parser.parseTokens {|"\x83qg\"nwgugfmfpzlrvty\"ryoxm"|} |> encode) ~expect:{|"\"\\x83qg\\\"nwgugfmfpzlrvty\\\"ryoxm\""|}

let partTwo input = 
  List.map input ~f:(fun str -> 
      let tokens = (Parser.parseTokens str) in
      String.length (encode tokens) - String.length str
    )
  |> List.fold ~init:0 ~f:( + )

let%test_unit _ = [%test_result : int] (partTwo [{|""|}; {|"abc"|}; {|"aaa\"aaa"|};{|"\x27"|}]) ~expect:19

let %expect_test _ =
  print_endline "Part 1";
  partOne puzzle_input   
  |> Int.to_string
  |> print_endline;

  print_endline "Part 2";
  partTwo puzzle_input
  |> Int.to_string
  |> print_endline;

  [%expect {|
    Part 1
    1342
    Part 2
    2074 |}]

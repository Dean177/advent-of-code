open Core

let puzzleInput = In_channel.read_all "./input.txt"

type event = 
  | ShiftStart of int
  | Wakes 
  | FallsAsleep
  [@@deriving sexp, variants]

type logLine = LogLine of (string * event) [@@deriving sexp]
  

module Parser = struct
  open Angstrom
  open Toolbox.Parser
  let timestamp = char '[' *> (take_till (fun char -> char = ']'))
  let guardStart = shiftstart <$> (string "Guard #" *> integer <* string " begins shift")
  let sleeps = string "falls asleep" *> return FallsAsleep
  let wakes = string "wakes up" *> return Wakes

  let logLine =   
    timestamp >>= fun ts ->
    space *> (guardStart <|> sleeps <|> wakes) >>= fun event ->
    return (LogLine (ts, event))

  let parse = parse_string_exn (sep_by_newline logLine)
end

let%expect_test _ =  
  print_endline "Part 1";
  puzzleInput |> Parser.parse |> [%sexp_of : logLine list] |> print_s;
  print_endline "Part Two";
  [%expect {|
    Part 1
    Part Two
  |}]

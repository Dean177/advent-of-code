open Core

type direction = Left | Right [@@deriving sexp, variants]

type operation =
  | SwapPosition of int * int
  | SwapLetter of char * char
  | RotateSteps of direction * int
  | RotateOnChar of char
  | Reverse of int * int
  | Move of int * int
[@@deriving sexp, variants]

module Parser = struct
  open Angstrom
  let integer = take_while1 (function | '0' .. '9' -> true | _ -> false) >>| Int.of_string
  let position_parser = (string "position " *> integer)
  let letter_parser = (string "letter " *> any_char)  
  let left_or_right = (string "left" *> return Left) <|> (string "right" *> return Right)

  let swap_positions = swapposition <$> (string "swap " *> position_parser) <*> (string " with " *> position_parser)   
  let swap_letters = swapletter <$> (string "swap " *> letter_parser) <*> (string " with " *> letter_parser) 
  let rotate_steps = rotatesteps <$> (string "rotate " *> left_or_right) <*> (string " " *> integer <* string " steps")
  let rotate_on_char = rotateonchar <$> (string "rotate based on position of letter " *> any_char)
  let reverse_p = reverse <$> (string "reverse positions " *> integer) <*> (string " through " *> integer)
  let move_p = move <$> (string "move position " *> integer) <*> (string " to position " *> integer) 

  let operation = many (char ' ') *> (
      swap_positions <|>
      swap_letters <|>
      rotate_steps <|>
      rotate_on_char <|>
      reverse_p <|>
      move_p
    )

  let operations = sep_by (char '\n') operation

  let parse str = parse_string operations str |> Result.ok_or_failwith

  let test_input = 
    {|swap position 4 with position 0
      swap letter d with letter b
      reverse positions 0 through 4
      rotate left 1 steps
      move position 1 to position 4
      move position 3 to position 0
      rotate based on position of letter b
      rotate based on position of letter d|}

  let%expect_test _ =
    parse test_input
    |> [%sexp_of : operation list]
    |> print_s;
    [%expect {|
      ((SwapPosition 4 0) (SwapLetter d b) (Reverse 0 4) (RotateOnIndex Left 1)
       (Move 1 4) (Move 3 0) (RotateOnChar b) (RotateOnChar d)) |}]

end

let rec apply str = function
| SwapPosition (x, y) -> (
  let x_val = Array.get str x in
  let y_val = Array.get str y in
  Array.set str y x_val;
  Array.set str x y_val;
  str
)
| SwapLetter (a, b) -> (
  let (a_idx, _) = Array.findi_exn str ~f:(fun _ chr -> chr = a) in
  let (b_idx, _) = Array.findi_exn str ~f:(fun _ chr -> chr = b) in
  apply str (SwapPosition (a_idx, b_idx))  
)
| RotateSteps (_, _) -> str
| RotateOnChar chr_val -> (
    let (chr_idx, _) = Array.findi_exn str ~f:(fun _ chr -> chr = chr_val) in
    apply str (RotateSteps (Right, chr_idx))
)
| Reverse (start, fin) -> str
| Move (origin, destingation) -> (
  let orig_val = Array.get str origin in
  let pre = Array.slice str 0 origin in
  let post = Array.slice str origin (Array.length str) in
  str
)

let scrable str operations = List.fold_left operations ~init:str ~f:apply

let%expect_test _ =
  (In_channel.read_all "./statements.txt") 
  |> Parser.parse
  |> scrable (String.to_array "abcdefgh")
  |> [%sexp_of : char array]
  |> print_s;
  [%expect {|
    rotate right 1 step
    swap position 2 with position 4
    rotate based on position of letter g
    rotate left 4 steps
    swap position 6 with position 0
    swap letter h with letter a
    swap letter d with letter c
    reverse positions 2 through 4
    swap position 2 with position 4
    swap letter d with letter e
    reverse positions 1 through 5
    swap letter b with letter a
    rotate right 0 steps
    swap position 7 with position 3
    move position 2 to position 1
    reverse positions 2 through 5
    reverse positions 4 through 7
    reverse positions 2 through 7
    swap letter e with letter c
    swap position 1 with position 7
    swap position 5 with position 7
    move position 3 to position 6
    swap position 7 with position 2
    move position 0 to position 7
    swap position 3 with position 7
    reverse positions 3 through 6
    move position 0 to position 5
    swap letter h with letter c
    reverse positions 2 through 3
    swap position 2 with position 3
    move position 4 to position 0
    rotate based on position of letter g
    rotate based on position of letter g
    reverse positions 0 through 2
    swap letter e with letter d
    reverse positions 2 through 5
    swap position 6 with position 0
    swap letter a with letter g
    swap position 2 with position 5
    reverse positions 2 through 3
    swap letter b with letter d
    reverse positions 3 through 7
    swap position 2 with position 5
    swap letter d with letter b
    reverse positions 0 through 3
    swap letter e with letter g
    rotate based on position of letter h
    move position 4 to position 3
    reverse positions 0 through 6
    swap position 4 with position 1
    swap position 6 with position 4
    move position 7 to position 5
    swap position 6 with position 4
    reverse positions 5 through 6
    move position 0 to position 6
    swap position 5 with position 0
    reverse positions 2 through 5
    rotate right 0 steps
    swap position 7 with position 0
    swap position 0 with position 2
    swap position 2 with position 5
    swap letter h with letter c
    rotate left 1 step
    reverse positions 6 through 7
    swap letter g with letter a
    reverse positions 3 through 7
    move position 2 to position 4
    reverse positions 0 through 6
    rotate based on position of letter g
    swap position 0 with position 6
    move position 2 to position 0
    rotate left 3 steps
    reverse positions 2 through 5
    rotate based on position of letter a
    reverse positions 1 through 4
    move position 2 to position 3
    rotate right 2 steps
    rotate based on position of letter f
    rotate based on position of letter f
    swap letter g with letter a
    rotate right 0 steps
    swap letter f with letter h
    swap letter f with letter b
    swap letter d with letter e
    swap position 0 with position 7
    move position 3 to position 0
    swap position 3 with position 0
    rotate right 4 steps
    rotate based on position of letter a
    reverse positions 0 through 7
    rotate left 6 steps
    swap letter d with letter h
    reverse positions 0 through 4
    rotate based on position of letter f
    move position 5 to position 3
    move position 1 to position 3
    move position 6 to position 0
    swap letter f with letter c
    rotate based on position of letter h
    reverse positions 6 through 7 |}]
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

  let operation = (
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
      ((SwapPosition 4 0)) |}]
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
| RotateSteps (Right, steps) -> if (steps = 0) then str else (
  let n = steps % Array.length str in
  let len = Array.length str in
  apply 
    (Array.append [|Array.get str (len - 1)|] (Array.slice str 0 (len - 1)))
    (RotateSteps (Right, n - 1))
)
| RotateSteps (Left, steps) -> if (steps = 0) then str else (
  let n = steps % Array.length str in
  let len = Array.length str in
  apply 
    (Array.append (Array.slice str 1 len) [|Array.get str 0|])
    (RotateSteps (Left, n - 1))
)
| RotateOnChar chr_val -> (
    let (chr_idx, _) = Array.findi_exn str ~f:(fun _ chr -> chr = chr_val) in
    let rotations = 1 + chr_idx + (if chr_idx >= 4 then 1 else 0) in
    apply str (RotateSteps (Right, rotations))
)
| Reverse (start, fin) -> (
  let pre = if start = 0 then [||] else Array.slice str 0 start in
  let mid = Array.slice str start (fin + 1) in
  let post = Array.slice str (fin + 1) (Array.length str) in
  Array.rev_inplace mid;
  Array.concat [pre; mid; post]
)
| Move (origin, dest) -> (
  let len = Array.length str in
  let orig_val = Array.get str origin in

  let pre = if origin = 0 then [||] else Array.slice str 0 origin in
  let post = Array.slice str (origin + 1) len in
  let without_orig = Array.append pre post in
  let without_len = Array.length without_orig in

  let pre_with = if dest = 0 then [||] else Array.slice without_orig 0 dest in 
  let post_with = if dest >= without_len then [||] else
    Array.slice without_orig dest (Array.length without_orig) in 
  Array.concat [pre_with;[|orig_val|];post_with]
)

let scrable str operations = List.fold_left operations ~init:str ~f:apply

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abcde") [SwapPosition (4,0)])
    ~expect:[|'e';'b';'c';'d';'a'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "ebcda") [SwapLetter ('d','b')])
    ~expect:[|'e';'d';'c';'b';'a'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "edcba") [Reverse (0, 4)])
    ~expect:[|'a';'b';'c';'d';'e'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abcde") [RotateSteps (Left, 1)])
    ~expect:[|'b';'c';'d';'e';'a'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "bcdea") [Move (1, 4)])
    ~expect:[|'b';'d';'e';'a';'c'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "bdeac") [Move (3, 0)])
    ~expect:[|'a';'b';'d';'e';'c'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abdec") [RotateOnChar 'b'])
    ~expect:[|'e';'c';'a';'b';'d'|]

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "ecabd") [RotateOnChar 'd'])
    ~expect:[|'d';'e';'c';'a';'b'|]

let%expect_test _ =
  (In_channel.read_all "./statements.txt") 
  |> Parser.parse
  (* |> scrable (String.to_array "abcdefgh") *)
  |> [%sexp_of : operation list]
  (* |> [%sexp_of : operation list] *)
  (* |> [%sexp_of : string] *)
  |> print_s;
  [%expect {|
    () |}]


  
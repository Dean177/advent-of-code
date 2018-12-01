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
  let rotate_steps = rotatesteps <$> (string "rotate " *> left_or_right) <*> (string " " *> integer <* (string " steps" <|> string " step"))
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
    ~expect:(String.to_array "ebcda")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "ebcda") [SwapLetter ('d','b')])
    ~expect:(String.to_array "edcba")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "edcba") [Reverse (0, 4)])
    ~expect:(String.to_array "abcde")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abcde") [RotateSteps (Left, 1)])
    ~expect:(String.to_array "bcdea")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "bcdea") [Move (1, 4)])
    ~expect:(String.to_array "bdeac")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "bdeac") [Move (3, 0)])
    ~expect:(String.to_array "abdec")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abdec") [RotateOnChar 'b'])
    ~expect:(String.to_array "ecabd")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "ecabd") [RotateOnChar 'd'])
    ~expect:(String.to_array "decab")

let%expect_test _ =
  (In_channel.read_all "./statements.txt") 
  |> Parser.parse
  |> scrable (String.to_array "abcdefgh")
  |> Array.to_list
  |> String.of_char_list
  |> print_endline;
  [%expect {| baecdfgh |}]

let unapply str = function
  | SwapPosition (x, y) -> apply str (SwapPosition (x, y))
  | SwapLetter (a, b) -> apply str (SwapLetter (a, b))
  | RotateSteps (Right, steps) -> apply str (RotateSteps (Left,steps))
  | RotateSteps (Left, steps) -> apply str (RotateSteps (Right, steps))
  | RotateOnChar chr_val -> (
      let (chr_idx, _) = Array.findi_exn str ~f:(fun _ chr -> chr = chr_val) in
      match chr_idx with
      | 0 -> apply str (RotateSteps (Left, 1))
      | 1 -> apply str (RotateSteps (Left, 1))
      | 2 -> apply str (RotateSteps (Right, 2))
      | 3 -> apply str (RotateSteps (Left, 2))
      | 4 -> apply str (RotateSteps (Right, 1))
      | 5 -> apply str (RotateSteps (Left, 3))
      | 6 -> str
      | 7 -> apply str (RotateSteps (Right, 4))
      | _ -> failwith ""
    )
  | Reverse (start, fin) -> apply str (Reverse (start, fin))
  | Move (origin, dest) -> apply str (Move (dest, origin))

let unscrable str operations = 
  List.fold_left (List.rev operations) ~init:str ~f:unapply

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "ebcda") [SwapPosition (4,0)])
    ~expect:(String.to_array "abcde")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "ebcda") [SwapLetter ('d','b')])
    ~expect:(String.to_array "edcba")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "abcde") [Reverse (0, 4)])
    ~expect:(String.to_array "edcba")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "bcdea") [RotateSteps (Left, 1)])
    ~expect:(String.to_array "abcde")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "bdeac") [Move (1, 4)])
    ~expect:(String.to_array "bcdea")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "abdec") [Move (3, 0)])
    ~expect:(String.to_array "bdeac")

let%test_unit _ =
  [%test_result : char array] 
    (unscrable (String.to_array "abcdefgh") [RotateOnChar 'b'])
    ~expect:(String.to_array "bcdefgha")

let%test_unit _ =
  [%test_result : char array] 
    (scrable (String.to_array "abcdefgh") [RotateOnChar 'd'])
    ~expect:(String.to_array "efghabcd")

(* I was confused by this, with length 8 Rotate on char is reversible, but with length 5 (Like the sample inputs) there can be multiple solutions *)
let%expect_test _ =
  (In_channel.read_all "./statements.txt") 
  |> Parser.parse
  |> unscrable (String.to_array "fbgdceah")
  |> Array.to_list
  |> String.of_char_list
  |> print_endline;
  [%expect {| cegdahbf |}]

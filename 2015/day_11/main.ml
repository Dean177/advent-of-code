open Core

let puzzleInput = "hxbxwxba" |> String.to_list

type password = char list
type t = int list

let passwordToT password = 
  List.rev_map password ~f:(fun char -> Char.to_int char - Char.to_int 'a')

let tToPassword digits = 
  List.rev_map digits ~f:(fun digit -> Char.of_int_exn (digit + Char.to_int 'a'))

let rec increment digits = match digits with
  | [] -> []
  | a :: rest -> if a >= 25 then (0 :: (increment rest)) else (a + 1) :: rest

let nextPassword password = password |> passwordToT |> increment |> tToPassword

let%test_unit _ =
  [%test_result : char list] (nextPassword ("abcdefgh" |> String.to_list)) ~expect:("abcdefgi" |> String.to_list)  

let rec containsRun password =  
  match password with
  | [] | _ :: [] | _ :: _ :: [] -> false
  | a :: b :: c :: rest -> 
    let isRun = (Char.to_int a + 1) = (Char.to_int b) && ((Char.to_int b) + 1) = (Char.to_int c) in
    isRun || containsRun (b :: c :: rest)

let containsIOL password =       
  List.exists password ~f:(fun char -> (char = 'i' || char = 'o' || char = 'l'))

let rec pairs password = 
  match password with 
  | [] | _ :: [] -> 0
  | a :: b :: rest -> if a = b then 1 + (pairs rest) else pairs (b :: rest) 

let isValidPassword password = 
  containsRun password && not (containsIOL password) && (pairs password >= 2)


let%test_unit _ =
  [%test_result : bool]  (isValidPassword ("hijklmmn" |> String.to_list)) ~expect:false

let%test_unit _ =
  [%test_result : bool]  (isValidPassword ("abbceffg" |> String.to_list)) ~expect:false

let%test_unit _ =
  [%test_result : bool]  (isValidPassword ("abcdffaa" |> String.to_list)) ~expect:true

let rec nextValidPassword password = 
  let next = nextPassword password in
  if isValidPassword next then
    next
  else
    nextValidPassword next

let%test_unit _ =
  [%test_result : char list]  (nextValidPassword ("abcdefgh" |> String.to_list)) ~expect:("abcdffaa" |> String.to_list)

let%test_unit _ =
  [%test_result : char list]  (nextValidPassword ("ghijklmn" |> String.to_list)) ~expect:("ghjaabcc" |> String.to_list)

let%expect_test _ =
  let partOne = nextValidPassword puzzleInput in
  partOne |> String.of_char_list |> print_endline;
  (nextValidPassword partOne) |> String.of_char_list |> print_endline;
  [%expect {|
    hxbxxyzz
    hxcaabcc |}]

open Core

let vowels = Char.Set.of_list (String.to_list "aeiou")
let vowel_count str = String.filter str ~f:(Set.mem vowels) |> String.length

let%test_unit _ = [%test_result : int] (vowel_count "dvszwmarrgswjxmb") ~expect:1

let rec sliding_pair = function 
  | [] -> []
  | _ :: [] -> []
  | x :: y :: ys -> (x, y) :: (sliding_pair (y :: ys))

let contains_double_letter str = 
  String.to_list str 
  |> sliding_pair
  |> List.exists ~f:(fun (a, b) -> a = b)

let%expect_test _ =
  sliding_pair (String.to_list "abcdde")
  |> [%sexp_of : (char * char) list]
  |> print_s;
  [%expect {| ((a b) (b c) (c d) (d d) (d e)) |}]

let%test_unit _ = [%test_result : bool] (contains_double_letter "xx") ~expect:true
let%test_unit _ = [%test_result : bool] (contains_double_letter "abcdde") ~expect:true
let%test_unit _ = [%test_result : bool] (contains_double_letter "jchzalrnumimnmhp") ~expect:false

let contains_disallowed_pair str = 
  List.exists ["ab"; "cd"; "pq"; "xy"] ~f:(fun pair -> String.is_substring str ~substring:pair)

let%test_unit _ = [%test_result : bool] (contains_disallowed_pair "haegwjzuvuyypxyu") ~expect:true

let is_nice str =
  vowel_count str >= 3 &&
  contains_double_letter str &&
  (not (contains_disallowed_pair str))

let%test_unit _ = [%test_result : bool] (is_nice "ugknbfddgicrmopn") ~expect:true
let%test_unit _ = [%test_result : bool] (is_nice "aaa") ~expect:true
let%test_unit _ = [%test_result : bool] (is_nice "jchzalrnumimnmhp") ~expect:false
let%test_unit _ = [%test_result : bool] (is_nice "haegwjzuvuyypxyu") ~expect:false
let%test_unit _ = [%test_result : bool] (is_nice "dvszwmarrgswjxmb") ~expect:false

let puzzle_input =  In_channel.read_all "./day_05.txt"  |> String.split ~on:'\n' 

let%expect_test _ =
  puzzle_input
  |> List.filter ~f:is_nice 
  |> List.length
  |> [%sexp_of : int] |> print_s;
  [%expect {| 238 |}]

module CharTuple = struct
  type t = char * char [@@deriving sexp]

  let compare (x0, y0) (x1, y1) =
    match Char.compare x0 x1 with
    | 0 -> Char.compare y0 y1
    | c -> c
end

module CharTupleS = Set.Make(CharTuple)

(* TODO this allows for overlapping  :-( *)

let rec contains_pair (x, y) = function 
  | [] | _ :: [] -> false
  | a :: b :: cs -> x = a && y = b || (contains_pair (x, y) (b :: cs))

let rec contains_repeated_pair = function
  | [] | _ :: [] | _ :: _ :: [] -> false
  | a :: b :: cs -> contains_pair (a, b) cs || contains_repeated_pair (b :: cs)


let%test_unit _ = [%test_result : bool] (contains_repeated_pair (String.to_list "qjhvhtzxzqqjkmpb")) ~expect:true

let rec sliding_trio = function 
  | [] -> []
  | _ :: [] -> []
  | _ :: _ :: [] -> []
  | x :: y :: z :: zs -> (x, y, z) :: (sliding_trio (y :: z :: zs))

let contains_spaced_repeat str = 
  String.to_list str 
  |> sliding_trio 
  |> List.exists ~f:(fun (x, _, y) -> x = y)

let%test_unit _ = [%test_result : bool] (contains_spaced_repeat "qjhvhtzxzqqjkmpb") ~expect:true

let is_nice_v2 str = contains_repeated_pair (String.to_list str) && contains_spaced_repeat str

let%test_unit _ = [%test_result : bool] (is_nice_v2 "qjhvhtzxzqqjkmpb") ~expect:true
let%test_unit _ = [%test_result : bool] (is_nice_v2 "xxyxx") ~expect:true
let%test_unit _ = [%test_result : bool] (is_nice_v2 "uurcxstgmygtbstg") ~expect:false
let%test_unit _ = [%test_result : bool] (is_nice_v2 "ieodomkazucvgmuy") ~expect:false

let%expect_test _ =
  puzzle_input
  |> List.filter ~f:is_nice_v2
  |> List.length
  |> [%sexp_of : int] |> print_s;
  [%expect {| 69 |}]

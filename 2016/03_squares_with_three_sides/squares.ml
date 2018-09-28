open Angstrom
open Core

let unwrap = function 
  | Ok a -> a
  | Error str -> failwith str

type triangle = (int * int * int) [@@deriving sexp]

let make_triangle a b c = (a,b,c)

let is_triangle (a,b,c) = (a + b > c) && (a + c > b) && (b + c > a)

let%test _ =
  is_triangle (5, 10, 25) = false

let%test _ =
  is_triangle (2, 4, 5) = true


let integer = Int.of_string <$> take_while1 (function '0'..'9' -> true | _ -> false)
let whitespace = skip_many (char ' ' <|> char '\t')
let parse_triangle = 
  whitespace *> (make_triangle <$> 
                 (integer <* whitespace) <*> 
                 (integer <* whitespace) <*> 
                 (integer <* whitespace))
let parse_triangles = sep_by (char '\n') parse_triangle

let%expect_test _ =
  parse_string parse_triangle "5  10  20" |> unwrap 
  |> [%sexp_of : triangle] |> Sexp.to_string |> print_endline;
  [%expect {| (5 10 20) |}]

let%expect_test _ =
  parse_string parse_triangle "  5  10  20" |> unwrap 
  |> [%sexp_of : triangle] |> Sexp.to_string |> print_endline;
  [%expect {| (5 10 20) |}]

let%expect_test _ =
  In_channel.read_all "./triangles.txt" |> parse_string parse_triangles |> unwrap 
  |> List.filter ~f:is_triangle
  |> List.length  
  |> Int.to_string 
  |> print_endline;
  [%expect {| 983 |}]

let create_triangle_columns triangles = match triangles with
  | [(x1, y1, z1); (x2, y2, z2); (x3, y3, z3);] -> [
      (make_triangle x1 x2 x3); 
      (make_triangle y1 y2 y3); 
      (make_triangle z1 z2 z3);
    ]
  | _ -> failwith ("Not provided with exactly 3 triangles, got " ^ ([%sexp_of : triangle list] triangles |> Sexp.to_string_hum ))

let transform_to_column_triangles triangle_list = 
  List.groupi ~break:(fun i _ _ -> i % 3 = 0) triangle_list 
  |> List.bind ~f:create_triangle_columns

let%expect_test _ =
  {eof|101 301 501
       102 302 502
       103 303 503
       201 401 601
       202 402 602
       203 403 603|eof}
  |> parse_string parse_triangles |> unwrap 
  |> transform_to_column_triangles
  |> List.filter ~f:is_triangle
  |> List.length  
  |> Int.to_string 
  |> print_endline;
  [%expect {| 6 |}]

let%expect_test _ =
  {eof|736   50  363
       657  707  408
       252  705   98
       532  173  878
       574  792  854
       157  737  303|eof}    
  |> parse_string parse_triangles |> unwrap 
  |> transform_to_column_triangles
  |> List.filter ~f:is_triangle
  |> List.length  
  |> Int.to_string 
  |> print_endline;
  [%expect {| 6 |}]

let%expect_test _ =
  In_channel.read_all "./triangles.txt" |> parse_string parse_triangles |> Result.ok_or_failwith 
  |> transform_to_column_triangles
  |> List.filter ~f:is_triangle
  |> List.length  
  |> Int.to_string 
  |> print_endline;
  [%expect {| 1836 |}]
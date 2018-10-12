open Core

let of_int_list = function 
  | l :: w :: h :: [] -> l, w, h
  | _ -> failwith ""

let surface_area (l, w, h) = 3*l*w + 2*w*h + 2*h*l
let ribbon_length (l, w, h) = 
  2 * l + 2 * w + (l * w * h)

module Parser = struct
  open Angstrom
  let integer = take_while1 (function | '0' .. '9' -> true | _ -> false) >>| Int.of_string
  let dimensions = List.sort ~compare:Int.compare <$> sep_by (char 'x') integer 
  let dimension_list = sep_by (char '\n') dimensions
  let parse str = parse_string dimension_list str |> Result.ok_or_failwith
end

let dimensions = In_channel.read_all "./day_02.txt" |> Parser.parse |> List.map ~f:of_int_list

let%expect_test _ =
  dimensions
  |> List.map ~f:surface_area
  |> List.fold ~init:0 ~f:( + )
  |> [%sexp_of : int] |> print_s;
  [%expect {|
    1598415 |}]

let%expect_test _ =
  dimensions
  |> List.map ~f:ribbon_length
  |> List.fold ~init:0 ~f:( + )
  |> [%sexp_of : int] |> print_s;
  [%expect {|
    3812909 |}]
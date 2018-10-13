open Core

module Matrix = struct
  open Owl  

  let with_slice_mut (r1, c1) (r2, c2) mat ~f = 
    let slice = Mat.get_slice [[r1; r2;];[c1;c2;]] mat |> f in
    Mat.set_slice [[r1; r2;];[c1;c2;]] mat slice

  let%expect_test _ =
    let arr = Mat.sequential 10 10 in
    with_slice_mut (0, 0) (4, 3) arr ~f:(Mat.map (fun el -> el *. -1.));
    Mat.to_arrays arr 
    |> [%sexp_of : float array array]
    |> print_s;
    [%expect {|
      ((-0 -1 -2 -3 4 5 6 7 8 9) (-10 -11 -12 -13 14 15 16 17 18 19)
       (-20 -21 -22 -23 24 25 26 27 28 29) (-30 -31 -32 -33 34 35 36 37 38 39)
       (-40 -41 -42 -43 44 45 46 47 48 49) (50 51 52 53 54 55 56 57 58 59)
       (60 61 62 63 64 65 66 67 68 69) (70 71 72 73 74 75 76 77 78 79)
       (80 81 82 83 84 85 86 87 88 89) (90 91 92 93 94 95 96 97 98 99)) |}]

end

module Parser = struct
  open Angstrom
  open Stdint
  let int_string = take_while1 (function | '0' .. '9' -> true | _ -> false)
  let integer = int_string >>| Int.of_string
  let uint16 = int_string >>| Uint16.of_string
  let whitespace = take_while (fun chr -> chr = ' ')

  let parse_string_exn p str = parse_string p str |> Result.ok_or_failwith

  let sep_by_newline p = sep_by (char '\n') p
end

module Point = struct
  module T = struct
    type t = (int * int) [@@deriving compare, hash, sexp_of]
    let create x y = x, y
    let levenstein (a, b) (x, y) = Int.abs (x - a) + Int.abs (y - b)
  end
  include T
  include Comparator.Make(T)

  let equal a b = compare a b = 0

  let to_string (x, y) = String.concat ["("; Int.to_string x; ","; Int.to_string y;")"]  
end

open Core

module T = struct
  type t = (int * int) [@@deriving compare, hash, sexp_of]
  let create x y = x, y
  let levenstein (a, b) (x, y) = Int.abs (x - a) + Int.abs (y - b)
end
include T
include Comparator.Make(T)

let equal a b = compare a b = 0

let to_string (x, y) = String.concat ["("; Int.to_string x; ","; Int.to_string y;")"]  

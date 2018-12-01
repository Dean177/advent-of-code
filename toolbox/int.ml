open Core

include Core.Int

let of_char c = String.of_char c |> Int.of_string

module List = struct
  let sum l = List.fold ~init:0 ~f:( + ) l

  let maximum l =
    List.fold l ~init:None ~f:(fun acc x -> 
        Some (Option.fold acc ~init:x ~f:(fun x' y -> Int.max x' y)))

  let minimum l = 
    List.fold l ~init:None ~f:(fun acc x -> 
        Some (Option.fold acc ~init:x ~f:(fun x' y -> Int.min x' y)))
end

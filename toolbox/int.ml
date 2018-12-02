open Core

include Core.Int

let of_char c = String.of_char c |> Int.of_string

open Core
open Angstrom

let isAlpha: char -> bool = function | 'a' .. 'z' -> true | _ -> false

let isHex: char -> bool = function 
  | '0' .. '9' | 'a' .. 'f' -> true 
  | _ -> false

let bracketed (p: 'a Angstrom.t) : 'a Angstrom.t = 
  (char '(') *> p <* (char ')')

let alphaCharacter: char Angstrom.t = 
  satisfy isAlpha

let hexCharacter: char Angstrom.t = 
  satisfy isHex

let int_string: string Angstrom.t = 
  take_while1 (function | '0' .. '9' -> true | _ -> false)

type sign = Positive | Negative
let signed: sign Angstrom.t =  option (Positive) (char '-' *> return Negative) 

let integer = int_string >>| Int.of_string  

let signed_integer = 
  signed >>= fun sign ->
  integer >>= fun int ->
  match sign with 
  | Positive -> return int
  | Negative -> return (-1 * int)  


let newline = char '\n'
let space = char ' '
let whitespace = take_while (fun chr -> chr = ' ')

let parse_string_exn p str = parse_string p str |> Result.ok_or_failwith

let sep_by_newline p = sep_by newline p

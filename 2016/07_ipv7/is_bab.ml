open Core

type ip_sequence = 
  | Regular of string
  | Hypernet of string

type ip = ip_sequence list

let bab str = 
  String.slice str 1 3 ^ String.slice str 1 2
  
let bab str = 
  String.slice str 1 3 ^ String.slice str 1 2

let is_bab str =
  Array.existsi (Array.slice str 0 ((Array.length str) - 3)) ~f:(fun i _ -> )

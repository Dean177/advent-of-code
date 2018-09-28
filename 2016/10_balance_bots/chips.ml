// https://medium.com/@__cpg/advent-of-code-2016-day10-6f67c44c2056
open Core

type microchip = int [@@deriving sexp]

type bot = { low: microchip option; high: microchip option; } [@@deriving sexp]
let empty_bot = {low = None; high=None;} 

type chip = High | Low [@@deriving sexp, variants]

type destination =  Bot of int | Output of int [@@deriving sexp, variants]

type instruction = 
  | Give of {
      giver: int;
      high_receiver: destination;
      low_receiver: destination;
    }
  | Receive of {
      value: microchip;
      destination: destination;
    }
[@@deriving sexp, variants]

module Parser = struct 
  open Angstrom
  let unwrap = function 
    | Ok a -> a
    | Error str -> failwith str

  let make_give bot_id chip_a destination_a chip_b destination_b = 
    match (chip_a, chip_b) with 
    | High, _ -> Give { giver = bot_id; high_receiver = destination_a; low_receiver = destination_b }
    | Low, _ -> Give { giver = bot_id; high_receiver = destination_b; low_receiver = destination_a }

  let make_receive value destination = Receive { value; destination}

  let integer = Int.of_string <$> take_while1 (function | '0'..'9' -> true | _ -> false)
  let which_chip = (string "low" *> return low) <|> (string "high" *> return high)
  let destination = 
    (bot <$> string "bot " *> integer) <|> 
    (output <$> (string "output " *> integer))

  let give_instr = make_give <$>
                   (string "bot " *> integer) <*> 
                   (string " gives " *> which_chip) <*> 
                   (string " to " *> destination) <*> 
                   (string " and " *> which_chip) <*>
                   (string " to " *> destination)

  let receive_instr = make_receive <$> 
                      (string "value " *> integer) <*> 
                      (string " goes to " *> destination)

  let instruction = give_instr <|> receive_instr
  let pp instr = 
    instr |> [%sexp_of : instruction] |> Sexp.to_string_hum |> print_endline

  let instructions = sep_by end_of_line instruction
  let parse input_string = parse_string instructions input_string |> unwrap 

  let%expect_test _ = 
    parse_string instruction "value 5 goes to bot 2" |> unwrap |> pp;
    [%expect {| (Receive (value 5) (destination (Bot 2))) |}]

  let%expect_test _ = 
    parse_string instruction "bot 2 gives low to bot 1 and high to bot 0" |> unwrap |> pp;
    [%expect {| (Give (giver 2) (high_receiver (Bot 0)) (low_receiver (Bot 1))) |}]

  let%expect_test _ =     
    parse_string instruction "bot 0 gives low to output 2 and high to output 0" |> Result.ok_or_failwith |> pp;
    [%expect {| (Give (giver 0) (high_receiver (Output 0)) (low_receiver (Output 2))) |}]

  let test_instructions = {eof|value 5 goes to bot 2
                               bot 2 gives low to bot 1 and high to bot 0
                               value 3 goes to bot 1
                               bot 1 gives low to output 1 and high to bot 0
                               bot 0 gives low to output 2 and high to output 0
                               value 2 goes to bot 2|eof} 
  (* 
  let%expect_test _ = 
    parse test_instructions
    |> [%sexp_of : instruction list] |> Sexp.to_string_hum 
    |> print_endline;
    [%expect {|
      ((Receive (value 5) (destination (Bot 2)))
       (Give (giver 2) (high_receiver (Bot 0)) (low_receiver (Bot 1)))
       (Receive (value 3) (destination (Bot 1)))
       (Give (giver 1) (high_receiver (Bot 0)) (low_receiver (Output 1)))
       (Give (giver 0) (high_receiver (Output 0)) (low_receiver (Output 2)))
       (Receive (value 2) (destination (Bot 2)))) |}] *)
end

let accept (chip_value : microchip) (some_bot : bot option) : bot = 
  let {low;high;} = match some_bot with | None -> empty_bot | Some b -> b in      
  match (low, high) with
  | (None, None) -> {low; high = Some chip_value}
  | (None, Some old_high) -> (
      if (chip_value > old_high) then {low=high; high = Some chip_value}
      else {low=(Some chip_value); high;})
  | (Some _, None) -> failwith "Unexpected bot state"
  | (Some old_low, Some old_high) -> 
    if chip_value < old_low then {low=Some chip_value;high;}
    else if chip_value > old_high then {low; high=Some chip_value}
    else {low;high}

let%expect_test _ = 
  accept 3 (Some {high=(Some 5);low=None}) |> [%sexp_of : bot] |> Sexp.to_string_hum |> print_endline;
  [%expect {| ((low (3)) (high (5))) |}]

let send (chip_value : microchip) destination (output, bots) =
  match destination with 
  | Output output_id -> (Map.set output ~key:output_id ~data:chip_value, bots)
  | Bot bot_id -> (output, Map.update bots bot_id ~f:(accept chip_value))

let execute ((output, bots): (int Int.Map.t * bot Int.Map.t)) = function  
  | Receive { value; destination; } ->  send value destination (output, bots)
  | Give { giver; high_receiver; low_receiver; } -> (match Map.find bots giver with
      | None -> (output, bots)
      | Some { low=(Some low_value); high=(Some high_value); } -> (
          print_string (
            "bot: " ^ (Int.to_string giver) ^ 
            " chip_values: " ^ (Int.to_string low_value) ^  " " ^ (Int.to_string low_value));
          (output, Map.set bots ~key:giver ~data:empty_bot)
          |> send low_value low_receiver 
          |> send high_value high_receiver
        )
      | Some _ -> (output, bots)
    )

let%expect_test _ =
  Parser.parse Parser.test_instructions
  |> List.fold 
    ~init:(Int.Map.empty, (Int.Map.of_alist_exn [(1, {high=(Some 3);low=None}); (2, {high= Some 5;low= Some 2})])) 
    ~f:execute
  |> [%sexp_of : (int Int.Map.t * bot Int.Map.t)] |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    bot: 2 chip_values: 2 2bot: 1 chip_values: 2 2bot: 0 chip_values: 3 3(((0 5) (1 2) (2 3))
     ((0 ((low ()) (high ()))) (1 ((low ()) (high ())))
      (2 ((low ()) (high (2)))))) |}]

let%expect_test _ =
  In_channel.read_all "./chip_distribution.txt" |> Parser.parse 
  |> List.fold 
    ~init:(Int.Map.empty, Int.Map.empty) 
    ~f:execute
  (* ~f:(fun state instruction ->  *)
  (* state |> [%sexp_of : (int Int.Map.t * bot Int.Map.t)] |> Sexp.to_string_hum |> print_endline;     *)
  (* instruction |> Parser.pp; *)
  (* execute state instruction) *)
  |> snd |> [%sexp_of : (bot Int.Map.t)] |> print_s
  ;
  [%expect {|
    ((17 ((low (47)) (high (67)))) (24 ((low ()) (high (71))))
     (33 ((low ()) (high (61)))) (45 ((low ()) (high (3))))
     (54 ((low ()) (high (13)))) (56 ((low ()) (high (59))))
     (62 ((low ()) (high (19)))) (80 ((low ()) (high (7))))
     (84 ((low ()) (high (2)))) (90 ((low ()) (high (43))))
     (104 ((low ()) (high (41)))) (114 ((low ()) (high (23))))
     (146 ((low ()) (high (53)))) (157 ((low ()) (high (5))))
     (159 ((low ()) (high (11)))) (167 ((low ()) (high (31))))
     (187 ((low ()) (high (17)))) (203 ((low ()) (high (73))))
     (208 ((low ()) (high (37)))) (209 ((low ()) (high (29))))) |}]

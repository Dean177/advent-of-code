open Core

let puzzleInput = In_channel.read_all "./day_01.txt"

type instruction = {
  register: string;
  direction: int -> int;
  move_amount: int;
  comparison_register: string;
  comparison_fun: int -> int -> bool;
  comparison_value: int;
}

let parseComparisonFunction = function
  | "<" -> ( < )
  | "<=" -> ( <= )
  | ">" -> ( > )
  | ">=" -> ( >= )
  | "==" -> ( = )
  | "!=" -> ( <> )
  | other -> failwith other

let parse input = 
  String.split_lines input
  |> List.map ~f:(fun line ->
      let words = Toolbox.String.words line in
      match words with 
      (* ioe dec 890 if qk > -10 *)
      | register :: direction :: move_amount :: _ :: comparison_register :: comparison_fun :: comparison_value :: [] -> {
          register;
          direction = (match direction with 
              | "inc" -> ( + ) 1
              | "dec" -> ( - ) 1
              | other -> failwith other);
          move_amount = Int.of_string move_amount;
          comparison_register;
          comparison_fun = parseComparisonFunction comparison_fun;
          comparison_value =  Int.of_string comparison_value;
        }
      | _ -> failwith (String.concat words)    
    )

let reigsterValue registers register = 
  Map.find registers register |> Option.value ~default:0

let partOne instructions = 
  let registers = ref String.Map.empty in
  let eval ins = 
    let comparisonRegisterValue = reigsterValue !registers ins.comparison_register in
    if (ins.comparison_fun comparisonRegisterValue ins.comparison_value) then
      registers := Map.update !registers ins.register ~f:(function
          | None -> ins.direction 0
          | Some v -> ins.direction v
        )
    else 
      ()
  in
  List.iter instructions ~f:eval;
  Map.fold !registers ~init:None ~f:(fun ~key:_ ~data max -> 
      match max with 
      | None -> Some data
      | Some v -> Some (if data > v then data else data)) 
  |> Option.value_exn

let%expect_test _ =  
  print_endline "Part 1";
  partOne (parse puzzleInput)
  |> Int.to_string 
  |> print_endline;

  print_endline "Part 2";
  [%expect {|
    Part 1
    4902
    Part 2 
    7037|}]

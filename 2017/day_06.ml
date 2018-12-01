open Core

let puzzleInput = "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
let parse input = 
  input
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
  |> Array.of_list

let rec incrementRegisters registers ~startingPosition ~total =  
  if total = 0 then () else (
    registers.(startingPosition) <- registers.(startingPosition) + 1;
    let nextPosition = 
      if startingPosition = (Array.length registers - 1) then 
        0 
      else 
        startingPosition + 1
    in
    incrementRegisters 
      registers
      ~startingPosition:nextPosition
      ~total:(total - 1)
  )

let redistributeRegisters registers register =
  let total = registers.(register) in
  let startingPosition = if register = (Array.length registers - 1) then 0 else register + 1 in
  registers.(register) <- 0;
  incrementRegisters registers ~startingPosition ~total;
  ()

let findMaxRegisterIndex registers = 
  Array.foldi registers ~init:None ~f:(
    fun i current_max current_value -> 
      match current_max with
      | None -> Some (i, current_value)
      | Some (_, value) -> (
          if current_value > value then
            Some (i, current_value)
          else
            current_max
        )
  )
  |> Option.value_exn
  |> fst

module IntArray = struct
  module T = struct
    type t = int array [@@deriving sexp]
    let compare = Array.compare Int.compare
  end 
  include T
  include Comparator.Make(T)
end 

let rec partOne registers ~previousStates ~totalCycles =     
  if Set.mem previousStates registers then (
    totalCycles
  ) else (
    let seenStates = Set.add previousStates (Array.copy registers) in
    let maxRegisterIndex = findMaxRegisterIndex registers in
    redistributeRegisters registers maxRegisterIndex;
    partOne registers ~previousStates:seenStates ~totalCycles:(totalCycles + 1)
  )

let rec partTwo registers ~previousStates ~currentCycle =     
  match Map.find previousStates registers with
  | Some cycle -> (currentCycle - cycle)
  | None -> (
      let maxRegisterIndex = findMaxRegisterIndex registers in
      let registerCopy: IntArray.t = Array.copy registers in
      let seenStates = 
        Map.set previousStates ~key:registerCopy ~data:currentCycle 
      in
      redistributeRegisters registers maxRegisterIndex;

      partTwo 
        registers 
        ~previousStates:seenStates
        ~currentCycle:(currentCycle + 1)
    )

let%expect_test _ =
  print_endline "Part One";
  partOne (parse puzzleInput) ~previousStates:(Set.empty (module IntArray)) ~totalCycles:0
  |> Int.to_string 
  |> print_endline;

  print_endline "Part Two";
  partTwo (parse puzzleInput) ~previousStates:(Map.empty (module IntArray)) ~currentCycle:0
  |> Int.to_string 
  |> print_endline;

  [%expect {|
    Part One
    11137
    Part Two
    1037 |}]

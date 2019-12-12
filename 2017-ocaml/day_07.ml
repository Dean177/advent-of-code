open Core

let puzzleInput = In_channel.read_all "./day_07.txt" 

let testInput = "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"

type program = { name: string; weight: int; child_names: string list }
let to_string {name;weight;child_names} = 
  String.concat [name; " ("; Int.to_string weight; ")"; " -> "; String.concat ~sep:", " child_names ]

module Parser = struct
  open Angstrom
  open Toolbox.Parser

  let children: string list Angstrom.t = 
    string " -> " *> 
    sep_by1 (string ", ") (many alphaCharacter >>| String.of_char_list)

  let disc = 
    (many alphaCharacter <* space) >>= fun nameChars -> 
    bracketed integer >>= fun weight ->
    option [] children >>= fun child_names ->
    return { 
      name = String.of_char_list nameChars; 
      weight; 
      child_names;
    }

  let parse_line str = parse_string_exn disc str
  let parse str = parse_string_exn (sep_by_newline disc) str
end

let rec findBase childNameToParentName currentName =
  match Map.find childNameToParentName currentName with 
  | Some {name; _} -> findBase childNameToParentName name
  | None -> currentName

let partOne input = 
  ignore input;
  let childNameToParent: program String.Map.t = 
    List.fold input 
      ~init:String.Map.empty
      ~f:(fun childToParent ({child_names; _} as parent) ->
          List.fold child_names ~init:childToParent ~f:(fun m childName ->
              Map.set m ~key:childName ~data:parent
            )
        ) 
  in
  findBase childNameToParent (List.hd_exn input).name

let partTwo input = ignore input; failwith "TODO"

let%expect_test _ =
  print_endline "Part One";
  Parser.parse testInput 
  |> partOne
  |> print_endline;

  print_endline "Part Two";
  (* partTwo (Parser.parse puzzleInput)
     |> Int.to_string 
     |> print_endline; *)

  [%expect {|
    Part One
    airlri
    Part Two
    1206 |}]

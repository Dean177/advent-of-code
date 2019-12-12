open Core

let initialState = 
  "##..#..##.#....##.#..#.#.##.#.#.######..##.#.#.####.#..#...##...#....#....#.##.###..#..###...#...#.."
  |> String.to_array

let puzzleInput = In_channel.read_all "./input.txt"

let parse str = 
  String.split_lines str 
  |> List.map ~f:(fun line -> 
      let words = String.split line ~on:' ' in
      match words with 
      | llcrr :: "=>" :: result :: [] -> (llcrr, result)
      | _ -> failwith ("Unrecognized input: " ^ line)
    )
  |> String.Map.of_alist

let slice str startingAt = 
  if (startingAt < 2) then 
    String.slice str startingAt (startingAt + 2)
  else if (startingAt )


let generation charr = 
  Array.mapi charr

let%expect_test _ =  
  print_endline "Part 1";
  print_endline "Part Two";
  [%expect {|
    Part 1
    Part Two
  |}]

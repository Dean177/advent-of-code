open Core

(* The prequel to this solution contained working through some pen & paper examples *)
let winner x = 
  let rec largest_power_of_two_less_than_x n = 
    let res = Int.pow 2 n in    
    if res = x then res
    else if res > x then Int.pow 2 (n - 1)
    else largest_power_of_two_less_than_x (n + 1)
  in

  let power = largest_power_of_two_less_than_x 1 in  
  ((x - power) * 2) + 1

let%expect_test _ =  
  winner 3012210 |> Int.to_string |> print_endline;
  [%expect {| 1830117 |}]


let survivor x = 
  let rec steal i elves =
    let number_of_elves = Set.length elves in
    if number_of_elves <= 1 then Int.Set.min_elt_exn elves 
    else (      
      let j = (i + (number_of_elves / 2)) mod number_of_elves in
      let new_elves = Int.Set.remove_index elves j in
      steal ((if j < i then i else i + 1) mod (Set.length new_elves)) new_elves
    )
  in
  steal 0 (Int.Set.of_array (Array.init x ~f:(( + ) 1)))

let%expect_test _ =  
  survivor 3012210 |> Int.to_string |> print_endline;
  [%expect {| 1417887 |}]
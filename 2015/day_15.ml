open Core

type ingredient = string * ((string * int) list)
type recipie = (ingredient * int) list

let frosting : ingredient = ("Frosting", [("capacity", 4); ("durability", -2); ("flavor", 0); ("texture", 0); ("calories", 5)])
let candy : ingredient = ("Candy", [("capacity", 0); ("durability", 5); ("flavor", -1); ("texture", 0); ("calories", 8)])
let butter : ingredient = ("Butterscotch",[("capacity", -1); ("durability", 0); ("flavor", 5); ("texture", 0); ("calories", 6)])
let sugar : ingredient = ("Sugar", [("capacity", 0); ("durability", 0); ("flavor", -2); ("texture", 2); ("calories", 1)])

let score (ingredients : recipie) : int = 
  ingredients
  |> List.map ~f:(fun ((ingredient_name, props), qty) -> 
      (ingredient_name, (
          List.map props ~f:(fun (prop_name, prop_val) -> (prop_name, prop_val * qty))
          |> List.filter ~f:(fun (prop_name, _) -> prop_name <> "calories")
        ))
    )
  |> List.fold ~init:(String.Map.empty) ~f:(fun totals (_, props) -> (
        List.fold props ~init:totals ~f:(fun total_n (prop_name, score) ->
            Map.update total_n prop_name ~f:(function | None -> score | Some s -> s + score)
          )
      ))
  |> Map.data
  |> List.fold ~init:1 ~f:(fun total sum -> total * (Int.max 0 sum))

let possible_recipies n = 
  let recipies = ref [] in
  for i = 0 to n do
    for j = 0 to n - i do
      for k = 0 to (n - i - j) do
        let l = (n - i - j - k) in
        recipies := [(frosting, i); (candy, j); (butter, k); (sugar, l)] :: !recipies;
      done
    done
  done;
  !recipies

let%expect_test _ =
  possible_recipies 100 
  |> List.map ~f:score 
  |> List.max_elt ~compare:Int.compare 
  |> Option.value_exn
  |> Int.to_string |> print_endline;
  [%expect {| 18965440 |}]

let total_calories (ingredients : recipie) : int = 
  ingredients
  |> List.map ~f:(fun ((_, props), qty) -> (
        let (_, cals) = List.find_exn props ~f:(fun (prop_name, _) -> prop_name = "calories") in  
        cals * qty
      ))
  |> List.fold ~init:0 ~f:(fun total_calories calories -> total_calories + calories)

let%expect_test _ =
  possible_recipies 100 
  |> List.filter ~f:(fun r -> total_calories r = 500)
  |> List.map ~f:score 
  |> List.max_elt ~compare:Int.compare 
  |> Option.value_exn
  |> Int.to_string |> print_endline;
  [%expect {| 15862900 |}]

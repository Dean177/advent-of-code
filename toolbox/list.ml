open Core

include Core.List

let scan list ~init ~f =
  let (result, _) = List.fold list ~init:([init], init) ~f:(fun (accs, acc) li -> 
      let new_acc = f acc li in
      (new_acc :: accs, new_acc))
  in
  result

let permutations list =
  let lstar = Array.of_list list in
  let len = Array.length lstar in
  let ks = List.range 1 (len + 1) in
  let indices = Int.Set.of_list (List.range 0 len) in
  let perm i =
    let (v, _, res) =
      List.fold_right ks ~init:(i, indices, []) ~f:(fun k (v, indices, res) ->
          let ix = Option.value_exn (Int.Set.nth indices (v mod k)) in
          (v / k, Int.Set.remove indices ix, lstar.(ix) :: res)
        )
    in
    if v > 0 then None else Some (res, i + 1)
  in
  Sequence.unfold ~init:0 ~f:perm

let%expect_test _ =
  permutations [1;2;3;] |> Sequence.to_list |> [%sexp_of : int list list] |> print_s;
  [%expect {| ((3 2 1) (3 1 2) (2 1 3) (2 3 1) (1 3 2) (1 2 3)) |}]

let sum l = List.fold ~init:0 ~f:( + ) l

let maximum l =
  List.fold l ~init:None ~f:(fun acc x -> 
      Some (Option.fold acc ~init:x ~f:(fun x' y -> Int.max x' y)))

let minimum l = 
  List.fold l ~init:None ~f:(fun acc x -> 
      Some (Option.fold acc ~init:x ~f:(fun x' y -> Int.min x' y)))

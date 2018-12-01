open Core

include Core.List

let scan list ~init ~f =
  let (result, _) = List.fold list ~init:([init], init) ~f:(fun (accs, acc) li -> 
      let new_acc = f acc li in
      (new_acc :: accs, new_acc))
  in
  result

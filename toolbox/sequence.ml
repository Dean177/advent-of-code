open Core 

include Core.Sequence

let fold_until_exn t ~init ~f = 
  Core.Sequence.fold_until t ~init ~f ~finish:(fun _ -> 
      failwith "Reached end of sequence without returning a Stop"
    )
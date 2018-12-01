
(* Probably should have gone with http://caml.inria.fr/pub/docs/manual-ocaml/libref/Event.html#TYPEevent 🤦‍ *)
open Async
open Core

type 'a t = {
  id: int;
  queue: 'a Queue.t;
  blockers: 'a Ivar.t Queue.t;
}

let channel_id_sequence = ref 0

let create () =
  let id = !channel_id_sequence in
  channel_id_sequence := id + 1;
  { id; queue = Queue.of_list []; blockers = Queue.of_list []; }

let receive (channel : 'a t) : 'a Deferred.t = 
  match Queue.dequeue channel.queue with 
  | Some v -> Deferred.return v
  | None -> Deferred.create (fun ivar ->
      Queue.enqueue channel.blockers ivar
    )

let send (channel: 'a t) (value : 'a) : unit = 
  match Queue.dequeue channel.blockers with 
  | None -> Queue.enqueue channel.queue value
  | Some b -> Ivar.fill b value
(* 
let%expect_test _ = Thread_safe.block_on_async_exn (fun () ->
    let chan = create () in
    send chan 1;
    send chan 2;
    let%map value = receive chan in
    print_endline (Int.to_string value);
    [%expect {| 1 |}]
  ) *)
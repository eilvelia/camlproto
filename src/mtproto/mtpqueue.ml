open! Base

type 'a t = {
  queue: 'a Queue.t;
  mutable element_added: unit -> unit;
}

let noop () = ()

let create () = {
  queue = Queue.create (); (* TODO: maybe use plain lists instead *)
  element_added = noop;
}

let dequeue_all t =
  let list = Queue.to_list t.queue in
  Queue.clear t.queue;
  list

let add t x =
  Queue.enqueue t.queue x;
  t.element_added ()

let get t =
  let (promise, rs) = Lwt.task () in

  let list = dequeue_all t in

  begin match list with
  | [] -> t.element_added <- fun () ->
    Lwt.wakeup_later rs (dequeue_all t);
    t.element_added <- noop
  | _ -> Lwt.wakeup_later rs list
  end;

  promise

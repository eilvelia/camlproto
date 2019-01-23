open! Base

type 'a t = {
  mutable queue: 'a list;
  mutable element_added: unit -> unit;
}

let noop () = ()

let create () = {
  queue = [];
  element_added = noop;
}

let dequeue_all t =
  let output_list = t.queue in
  t.queue <- [];
  output_list

let add t x =
  t.queue <- x :: t.queue;
  t.element_added ()

let add_all t xs =
  t.queue <- xs @ t.queue;
  t.element_added ()

let get_all t =
  let (promise, rs) = Lwt.task () in

  let list = dequeue_all t in

  begin match list with
  | [] -> t.element_added <- fun () ->
    Lwt.wakeup_later rs (dequeue_all t);
    t.element_added <- noop
  | _ -> Lwt.wakeup_later rs list
  end;

  promise

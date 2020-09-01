open! Base

module type VertexMin = sig
  type t
  [@@deriving hash]
  include Comparator.S with type t := t
end

module type S = sig
  type vertex

  module Vertex : sig type t = vertex end
  module Edge : sig type t end

  type vs
  (** vertices *)

  type es
  (** edges *)

  type t

  val empty : t

  val length : t -> int

  val vertices : t -> Vertex.t list
  val edges : t -> (Vertex.t * Vertex.t) list

  val add_vertex : t -> Vertex.t -> t

  val add_edge : t -> Vertex.t -> Vertex.t -> t

  val tarjan : t -> Vertex.t list list
end

module Make (V : VertexMin) : S with type vertex = V.t = struct
  type vertex = V.t

  module Vertex = struct
    type t = V.t

    let compare t1 t2 = V.comparator.compare t1 t2
    let sexp_of_t t : Sexp.t = List [Atom "Vertex"; V.comparator.sexp_of_t t]
    let hash = V.hash

    include Comparable.Make(struct
        type nonrec t = t
        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  module Edge = struct
    type t = Vertex.t * Vertex.t
    [@@deriving compare, sexp_of]
    include Comparator.Make(struct
        type nonrec t = t
        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  type vs = Set.M(Vertex).t
  type es = Set.M(Edge).t
  type t = { vs : vs; es : es }

  let empty = { vs = Set.empty (module Vertex); es = Set.empty (module Edge) }

  let length g = Set.length g.vs

  let vertices g = Set.to_list g.vs
  let edges g = Set.to_list g.es

  let add_vertex g v =
    let g' = { g with vs = Set.add g.vs v } in
    g'

  let add_edge g v w =
    { g with es = Set.add g.es (v, w) }

  (** Tarjan's strongly connected components algorithm
    * https://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
    *)
  module Tarjan = struct
    type tvertex = {
      mutable index : int (* -1 if none *);
      mutable lowlink : int (* -1 if none *);
      mutable on_stack : bool;
    }
    [@@deriving sexp_of]

    let make_tvertex () = {
      index = -1;
      lowlink = -1;
      on_stack = false;
    }

    type mapping = (Vertex.t, tvertex) Hashtbl.t

    let tarjan (g : t) m : _ list list =
      let tget v = Hashtbl.find_or_add m v ~default:make_tvertex in
      let index = ref 0 in
      let stack = Stack.create () in
      let components = ref [] in
      let rec strongconnect (v : Vertex.t) : unit =
        let vt = tget v in
        vt.index <- !index;
        vt.lowlink <- !index;
        index := !index + 1;
        Stack.push stack v;
        vt.on_stack <- true;
        Set.iter g.es ~f:(function
            | (v', w) when Vertex.(v = v') ->
              let wt = tget w in
              if wt.index = -1 then begin
                strongconnect w;
                vt.lowlink <- min vt.lowlink wt.lowlink;
              end else if wt.on_stack then begin
                vt.lowlink <- min vt.lowlink wt.index;
              end
            | _ -> ());
        if vt.lowlink = vt.index then
          components := component v :: !components
      and component v : _ list =
        let w = Stack.pop_exn stack in
        let wt = tget w in
        wt.on_stack <- false;
        if Vertex.(v = w) then
          [w]
        else
          w :: component v
      in
      Set.iter g.vs ~f:(fun v -> if (tget v).index = -1 then strongconnect v);
      !components

    let run (g : t) : Vertex.t list list =
      let m : mapping = Hashtbl.create (module Vertex) in
      tarjan g m
  end

  let tarjan = Tarjan.run
end

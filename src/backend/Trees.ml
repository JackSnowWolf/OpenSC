open BinNums
open Datatypes
open Globalenvs
open Maps0
open Options

(** val map_error : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list option **)

let rec map_error f = function
| Coq_nil -> Some Coq_nil
| Coq_cons (a, rest) ->
  bind (map_error f rest) (fun rest' ->
    bind (f a) (fun a' -> Some (Coq_cons (a', rest'))))

(** val xtransl_tree :
    ('a1 -> 'a2 option) -> (positive, 'a1) prod list -> 'a2 PTree.t option **)

let rec xtransl_tree f = function
| Coq_nil -> Some PTree.empty
| Coq_cons (p, rest) ->
  let Coq_pair (k, a) = p in
  bind (f a) (fun b ->
    bind (xtransl_tree f rest) (fun rest0 -> Some (PTree.set k b rest0)))

(** val transl_tree :
    ('a1 -> 'a2 option) -> 'a1 PTree.t -> 'a2 PTree.t option **)

let transl_tree f t0 =
  xtransl_tree f (PTree.elements t0)

(** val partial_f : ('a1 -> 'a2 option) -> 'a1 option -> 'a2 option option **)

let partial_f f = function
| Some a' -> bind (f a') (fun b -> Some (Some b))
| None -> None

(** val transl_map :
    ('a1 -> 'a2 option) -> 'a1 option IntMap.t -> 'a2 option IntMap.t option **)

let transl_map f = function
| Coq_pair (o, t1) ->
  (match o with
   | Some _ -> None
   | None ->
     bind (transl_tree (partial_f f) t1) (fun tt -> Some (Coq_pair (None,
       tt))))

(** val xtransl_tree_keys_move :
    (positive -> 'a1 -> (positive, 'a2) prod option) -> (positive, 'a1) prod
    list -> 'a2 PTree.t option **)

let rec xtransl_tree_keys_move f = function
| Coq_nil -> Some PTree.empty
| Coq_cons (p, rest) ->
  let Coq_pair (k, a) = p in
  bind2 (f k a) (fun k' b ->
    bind (xtransl_tree_keys_move f rest) (fun rest0 -> Some
      (PTree.set k' b rest0)))

(** val transl_tree_keys_move :
    (positive -> 'a1 -> (positive, 'a2) prod option) -> 'a1 PTree.t -> 'a2
    PTree.t option **)

let transl_tree_keys_move f t0 =
  xtransl_tree_keys_move f (PTree.elements t0)

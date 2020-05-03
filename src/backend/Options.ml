open Datatypes

(** val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option **)

let bind a f =
  match a with
  | Some x -> f x
  | None -> None

(** val bind2 :
    ('a1, 'a2) prod option -> ('a1 -> 'a2 -> 'a3 option) -> 'a3 option **)

let bind2 a f =
  match a with
  | Some p -> let Coq_pair (x, y) = p in f x y
  | None -> None

(** val optional_filter : 'a1 option -> 'a1 list **)

let optional_filter = function
| Some a' -> Coq_cons (a', Coq_nil)
| None -> Coq_nil

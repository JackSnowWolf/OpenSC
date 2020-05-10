module D = Datatypes

let rec eval_nat = function
  | D.O -> 0
  | D.S rest -> (eval_nat rest) + 1

let rec caml_list = function
  | D.Coq_nil -> []
  | D.Coq_cons (a, l) -> a :: caml_list l

let caml_prod = function
  | D.Coq_pair (a, b) -> (a, b)

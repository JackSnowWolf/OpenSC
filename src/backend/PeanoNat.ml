open Datatypes
open Specif

module Nat =
 struct
  (** val leb : nat -> nat -> bool **)

  let rec leb n m =
    match n with
    | O -> Coq_true
    | S n' -> (match m with
               | O -> Coq_false
               | S m' -> leb n' m')

  (** val eq_dec : nat -> nat -> sumbool **)

  let rec eq_dec n m =
    match n with
    | O -> (match m with
            | O -> Coq_left
            | S _ -> Coq_right)
    | S n0 -> (match m with
               | O -> Coq_right
               | S m0 -> eq_dec n0 m0)
 end

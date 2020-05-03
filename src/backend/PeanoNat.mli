open Datatypes
open Specif

module Nat :
 sig
  val leb : nat -> nat -> bool

  val eq_dec : nat -> nat -> sumbool
 end

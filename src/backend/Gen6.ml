open AST
open Cop
open Datatypes
open Globalenvs
open Integers
open Language5
open Language6
open List0
open Maps0
open Options
open Semantics1
open Values
open Zpower

(** val methodical_fundefs : coq_function PTree.t -> code **)

let methodical_fundefs t0 =
  flat_map fn_code (map snd (PTree.elements t0))

(** val methodical_opt_function : coq_function option -> code **)

let methodical_opt_function = function
| Some f -> fn_code f
| None -> Coq_nil

(** val methodical_methods : coq_function option IntMap.t -> code **)

let methodical_methods methods =
  flat_map methodical_opt_function (map snd (PTree.elements (snd methods)))

(** val label_method_starts_with : coq_function -> label option **)

let label_method_starts_with m =
  match fn_code m with
  | Coq_nil -> None
  | Coq_cons (s, _) -> (match s with
                        | Slabel l -> Some l
                        | _ -> None)

(** val sg_val : Int.int -> coq_val **)

let sg_val sg =
  Vint (Int256.repr (Int.unsigned sg))

(** val methodical_multiplexer_body :
    Int.int list -> coq_function option IntMap.t -> code option **)

let rec methodical_multiplexer_body methods methoddefs =
  match methods with
  | Coq_nil -> Some Coq_nil
  | Coq_cons (sg, rest) ->
    bind (methodical_multiplexer_body rest methoddefs) (fun rest' ->
      bind (IntMap.get sg methoddefs) (fun m ->
        bind (label_method_starts_with m) (fun l -> Some (Coq_cons ((Sdup O),
          (Coq_cons ((Spush (Coq_inl (sg_val sg))), (Coq_cons ((Sbinop (Oeq,
          Coq_false)), (Coq_cons ((Spush (Coq_inr l)), (Coq_cons (Sjumpi,
          rest')))))))))))))

(** val methodical_main : program -> code option **)

let methodical_main p =
  let ge = fst p in
  let body = snd p in
  let methods = ge.Genv.genv_methods in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (methodical_multiplexer_body methods methoddefs)
    (fun multiplexer_body -> Some
    (app (methodical_opt_function constructor) (Coq_cons ((Slabel body),
      (Coq_cons ((Spush (Coq_inl (Vint
      (Int256.repr (two_power_nat sg_shift))))), (Coq_cons ((Spush z0),
      (Coq_cons (Scalldataload, (Coq_cons ((Sbinop (Odiv, Coq_false)),
      (app multiplexer_body (Coq_cons (Srevert,
        (app (methodical_methods methoddefs) (methodical_fundefs fundefs)))))))))))))))))

(** val methodical_genv : program -> genv option **)

let methodical_genv p =
  bind (methodical_main p) (fun main_code ->
    let ge = fst p in
    let body = snd p in
    let vars = ge.Genv.genv_vars in
    let defs = ge.Genv.genv_defs in
    (match label_verify main_code with
     | Coq_true ->
       Some { genv_vars = vars; genv_defs = defs; genv_main = main_code;
         genv_main_entrypoint = body }
     | Coq_false -> None))

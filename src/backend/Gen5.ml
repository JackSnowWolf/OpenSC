open AST
open Datatypes
open Globalenvs
open Language5
open Language0
open Maps0
open MemoryModel
open Options
open Semantics0
open Trees
open Values

(** val expressionless_expr : expr -> Language5.statement **)

let expressionless_expr = function
| Econst_int256 i -> Spush (Coq_inl (Vint i))
| Evar id -> Spush (Coq_inl (Vptr (Iident id)))
| Etempvar n -> Sdup n
| Ederef -> Ssload
| Eunop o -> Sunop o
| Ebinop (o, s) -> Sbinop (o, s)
| Ecall0 b -> Scall0 b
| Ecall1 b -> Scall1 b

(** val pops : nat -> Language5.statement list **)

let rec pops = function
| O -> Coq_nil
| S m -> Coq_cons (Language5.Spop, (pops m))

(** val cleanup : nat -> Language5.statement list **)

let cleanup n = match n with
| O -> Coq_nil
| S m -> Coq_cons ((Sswap m), (pops n))

(** val expressionless_rt : ret_type -> Language5.ret_type option **)

let expressionless_rt = function
| Tvoid_fun -> Some Tfun
| Tvoid_method -> Some Language5.Tvoid_method
| Terror -> None
| Tsome_fun -> Some Tfun
| Tsome_method -> Some Language5.Tsome_method
| _ -> Some Tconstructor

(** val fetch_args : nat -> nat -> Language5.statement list **)

let rec fetch_args count base =
  match count with
  | O -> Coq_nil
  | S n ->
    app (fetch_args n (S base)) (Coq_cons ((Spush (Coq_inl (Vint
      (call_data_arg_location base)))), (Coq_cons (Scalldataload, Coq_nil))))

(** val extract_lbl : typed_label -> label **)

let extract_lbl = function
| Linternal l' -> l'
| Lcall l' -> l'
| Lreturn l' -> l'

(** val expressionless_stm : statement -> Language5.statement list option **)

let expressionless_stm = function
| Sskip -> Some (Coq_cons (Language5.Sskip, Coq_nil))
| Srvalue e -> Some (Coq_cons ((expressionless_expr e), Coq_nil))
| Slvalue e -> Some (Coq_cons ((expressionless_expr e), Coq_nil))
| Spushvoid -> Some (Coq_cons ((Spush (Coq_inl Vunit)), Coq_nil))
| Spop -> Some (Coq_cons (Language5.Spop, Coq_nil))
| Sassign -> Some (Coq_cons (Ssstore, Coq_nil))
| Sset n -> Some (Coq_cons ((Sswap n), (Coq_cons (Language5.Spop, Coq_nil))))
| Sdone (n, rt) ->
  bind (expressionless_rt rt) (fun rt' -> Some
    (app (cleanup n) (Coq_cons ((Language5.Sdone rt'), Coq_nil))))
| Spushlabel l -> Some (Coq_cons ((Spush (Coq_inr (extract_lbl l))), Coq_nil))
| Slabel l -> Some (Coq_cons ((Language5.Slabel l), Coq_nil))
| Sjump -> Some (Coq_cons (Language5.Sjump, Coq_nil))
| Sjumpi -> Some (Coq_cons (Language5.Sjumpi, Coq_nil))
| Stransfer -> Some (Coq_cons (Language5.Stransfer, Coq_nil))
| Scallmethod (i, a, r) ->
  Some (Coq_cons ((Language5.Scallmethod (i, a, r)), Coq_nil))
| Slog -> Some (Coq_cons (Language5.Slog, Coq_nil))
| Srevert -> Some (Coq_cons (Language5.Srevert, Coq_nil))
| Sfetchargs n -> Some (Coq_cons (Language5.Sskip, (fetch_args n O)))

(** val expressionless_code : code -> Language5.code option **)

let rec expressionless_code = function
| Coq_nil -> Some Coq_nil
| Coq_cons (s, rest) ->
  bind (expressionless_stm s) (fun s' ->
    bind (expressionless_code rest) (fun rest' -> Some (app s' rest')))

(** val expressionless_function :
    coq_function -> Language5.coq_function option **)

let expressionless_function f =
  bind (expressionless_code (fn_code f)) (fun c -> Some c)

(** val expressionless_fundefs :
    coq_function PTree.t -> Language5.coq_function PTree.t option **)

let expressionless_fundefs t0 =
  transl_tree expressionless_function t0

(** val expressionless_methods :
    coq_function option IntMap.t -> Language5.coq_function option IntMap.t
    option **)

let expressionless_methods methods =
  transl_map expressionless_function methods

(** val expressionless_constructor :
    coq_function option -> Language5.coq_function option **)

let expressionless_constructor = function
| Some c -> bind (expressionless_function c) (fun f -> Some f)
| None -> None

(** val expressionless_genv : genv -> Language5.genv option **)

let expressionless_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (expressionless_fundefs fundefs) (fun fundefs0 ->
    bind (expressionless_methods methoddefs) (fun methoddefs0 ->
      bind (expressionless_constructor constructor) (fun constructor0 -> Some
        { Genv.genv_vars = vars; Genv.genv_funcs = funcs; Genv.genv_methods =
        methods; Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
        Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
        constructor0) })))

(** val expressionless_program : program -> Language5.program option **)

let expressionless_program = function
| Coq_pair (ge, body) ->
  bind (expressionless_genv ge) (fun cge ->
    match label_verify cge with
    | Coq_true -> Some (Coq_pair (cge, body))
    | Coq_false -> None)

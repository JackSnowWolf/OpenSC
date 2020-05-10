open AST
open BinNums
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language2
open Language
open Language0
open List0
open Maps0
open MemoryModel
open Nat0
open Options
open Semantics
open TempModel
open Trees

(** val stacked_expr :
    nat PTree.t -> Language.expr -> nat -> bool -> statement list option **)

let rec stacked_expr temps e s_extra rvalue =
  match e with
  | Language.Econst_int256 (i, _) ->
    (match rvalue with
     | Coq_true -> Some (Coq_cons ((Srvalue (Econst_int256 i)), Coq_nil))
     | Coq_false -> None)
  | Language.Evar (i, _) ->
    (match rvalue with
     | Coq_true -> None
     | Coq_false -> Some (Coq_cons ((Slvalue (Evar i)), Coq_nil)))
  | Language.Etempvar (i, _) ->
    (match rvalue with
     | Coq_true ->
       bind (PTree.get i temps) (fun ind -> Some (Coq_cons ((Srvalue
         (Etempvar (add s_extra ind))), Coq_nil)))
     | Coq_false -> None)
  | Language.Ederef (e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (stacked_expr temps e0 s_extra Coq_false) (fun e' -> Some
         (app e' (Coq_cons ((Srvalue Ederef), Coq_nil))))
     | Coq_false -> None)
  | Language.Eunop (o, e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (stacked_expr temps e0 s_extra Coq_true) (fun e' -> Some
         (app e' (Coq_cons ((Srvalue (Eunop o)), Coq_nil))))
     | Coq_false -> None)
  | Language.Ebinop (o, e1, e2, _) ->
    bind (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
      bind (stacked_expr temps e2 s_extra Coq_true) (fun e2' -> Some
        (app e2'
          (app e1' (Coq_cons
            ((match rvalue with
              | Coq_true -> Srvalue (Ebinop (o, Coq_false))
              | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
  | Language.Ecall0 (b, _) ->
    (match rvalue with
     | Coq_true -> Some (Coq_cons ((Srvalue (Ecall0 b)), Coq_nil))
     | Coq_false -> None)
  | Language.Ecall1 (b, e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (stacked_expr temps e0 s_extra Coq_true) (fun e' -> Some
         (app e' (Coq_cons ((Srvalue (Ecall1 b)), Coq_nil))))
     | Coq_false -> None)
  | _ -> None

(** val stacked_exprs :
    nat PTree.t -> Language.expr list -> nat -> statement list option **)

let rec stacked_exprs temps es s_extra =
  match es with
  | Coq_nil -> Some Coq_nil
  | Coq_cons (e, rest) ->
    bind (stacked_expr temps e (add (length rest) s_extra) Coq_true)
      (fun e' ->
      bind (stacked_exprs temps rest s_extra) (fun rest' -> Some
        (app rest' e')))

(** val stacked_optexpr :
    nat PTree.t -> Language.expr option -> statement list option **)

let stacked_optexpr temps = function
| Some e -> stacked_expr temps e O Coq_true
| None -> Some (Coq_cons (Spushvoid, Coq_nil))

(** val ident_indices :
    nat PTree.t -> ident list -> nat -> nat list option **)

let rec ident_indices temps a offset =
  match a with
  | Coq_nil -> Some Coq_nil
  | Coq_cons (i, rest) ->
    bind (PTree.get i temps) (fun t0 ->
      bind (ident_indices temps rest offset) (fun resti -> Some (Coq_cons
        ((add (add t0 offset) (length rest)), resti))))

(** val set_indices : nat list -> statement list **)

let set_indices inds =
  map (fun x -> Sset x) inds

(** val optident : nat PTree.t -> ident option -> statement option **)

let optident temps = function
| Some b -> bind (PTree.get b temps) (fun t0 -> Some (Sset t0))
| None -> Some Spop

(** val return_type : Language2.coq_function -> coq_type **)

let return_type =
  Language2.fn_return

(** val toreturn : Language2.coq_function -> bool option -> ret_type **)

let toreturn f = function
| Some b ->
  (match b with
   | Coq_true ->
     (match return_type f with
      | Tvoid -> Tvoid_method
      | Tarray (_, _) -> Terror
      | Tstruct (_, _) -> Terror
      | _ -> Tsome_method)
   | Coq_false ->
     (match return_type f with
      | Tvoid -> Tvoid_fun
      | Tarray (_, _) -> Terror
      | Tstruct (_, _) -> Terror
      | _ -> Tsome_fun))
| None ->
  (match return_type f with
   | Tvoid -> Tvoid_constructor
   | Tarray (_, _) -> Terror
   | Tstruct (_, _) -> Terror
   | _ -> Tsome_constructor)

(** val zero_stm : statement **)

let zero_stm =
  Srvalue (Econst_int256 Int256.zero)

(** val xzero_stms : nat -> statement list **)

let rec xzero_stms = function
| O -> Coq_nil
| S n -> app (xzero_stms n) (Coq_cons (zero_stm, Coq_nil))

(** val zero_stms : nat -> statement list **)

let zero_stms c = match c with
| O -> Coq_cons (Sskip, Coq_nil)
| S _ -> xzero_stms c

(** val z_stm : coq_Z -> statement **)

let z_stm z =
  Srvalue (Econst_int256 (Int256.repr z))

(** val stacked_code :
    nat PTree.t -> Language2.coq_function -> Language2.code -> code option **)

let rec stacked_code temps f = function
| Coq_nil -> Some Coq_nil
| Coq_cons (s, rest) ->
  bind (stacked_code temps f rest) (fun trest ->
    match s with
    | Language2.Sassign (lv, rv) ->
      bind (stacked_expr temps lv (S O) Coq_false) (fun lv' ->
        bind (stacked_expr temps rv O Coq_true) (fun rv' -> Some
          (app rv' (app lv' (Coq_cons (Sassign, trest))))))
    | Language2.Sset (i, rv) ->
      bind (PTree.get i temps) (fun t0 ->
        bind (stacked_expr temps rv O Coq_true) (fun rv' -> Some
          (app rv' (Coq_cons ((Sset t0), trest)))))
    | Language2.Scall (rv, dest, args, retlbl) ->
      bind (optident temps rv) (fun t0 ->
        bind (stacked_exprs temps args (S O)) (fun args' -> Some
          (app (Coq_cons ((Spushlabel (Lreturn retlbl)), args'))
            (app (Coq_cons ((Spushlabel (Lcall dest)), (Coq_cons (Sjump,
              (Coq_cons ((Slabel retlbl), Coq_nil)))))) (Coq_cons (t0,
              trest))))))
    | Language2.Sreturn rv ->
      bind (stacked_optexpr temps rv) (fun rv' -> Some (app rv' trest))
    | Language2.Sdone method0 ->
      Some (Coq_cons ((Sdone ((length (all_temps ftype (Obj.magic f))),
        (toreturn f method0))), trest))
    | Language2.Slabel lbl -> Some (Coq_cons ((Slabel lbl), trest))
    | Language2.Sjump lbl ->
      Some (Coq_cons ((Spushlabel (Linternal lbl)), (Coq_cons (Sjump,
        trest))))
    | Language2.Sjumpi (cond, lbl) ->
      bind (stacked_expr temps cond O Coq_true) (fun cond' -> Some
        (app cond' (Coq_cons ((Spushlabel (Linternal lbl)), (Coq_cons
          (Sjumpi, trest))))))
    | Language2.Stransfer (a, v, fail) ->
      bind (stacked_expr temps a (S (S (S (S (S O))))) Coq_true) (fun a' ->
        bind (stacked_expr temps v (S (S (S (S O)))) Coq_true) (fun v' ->
          Some (Coq_cons (zero_stm, (Coq_cons (zero_stm, (Coq_cons (zero_stm,
          (Coq_cons (zero_stm,
          (app v'
            (app a' (Coq_cons (zero_stm, (Coq_cons (Stransfer, (Coq_cons
              ((Srvalue (Eunop Onotbool)), (Coq_cons ((Spushlabel (Linternal
              fail)), (Coq_cons (Sjumpi, trest))))))))))))))))))))))
    | Language2.Scallmethod (a, rvs, sg, v, args, fail) ->
      bind (stacked_expr temps a (S (S (S (S (S O))))) Coq_true) (fun a' ->
        bind (stacked_expr temps v (S (S (S (S O)))) Coq_true) (fun v' ->
          bind (stacked_exprs temps args (S (S (S (S (S (S (S O))))))))
            (fun args' ->
            bind (ident_indices temps rvs (S O)) (fun rv_inds -> Some
              (let retcount = length rvs in
               let argcount = length args in
               Coq_cons ((z_stm (arglen argcount)), (Coq_cons
               ((z_stm (argpos retcount)), (Coq_cons
               ((z_stm (retlen retcount)), (Coq_cons ((z_stm retpos),
               (app v'
                 (app a' (Coq_cons (zero_stm,
                   (app args'
                     (app (Coq_cons ((Scallmethod (sg, argcount, retcount)),
                       (set_indices rv_inds)))
                       (app (Coq_cons ((Srvalue (Eunop Onotbool)), (Coq_cons
                         ((Spushlabel (Linternal fail)), (Coq_cons (Sjumpi,
                         Coq_nil)))))) trest)))))))))))))))))))
    | Language2.Slog e ->
      bind (stacked_expr temps e O Coq_true) (fun e' -> Some
        (app e' (Coq_cons (Slog, trest))))
    | Language2.Srevert -> Some (Coq_cons (Srevert, trest))
    | Language2.Sfetchargs fetch ->
      Some (Coq_cons
        ((match fetch with
          | Coq_true -> Sfetchargs (length (some_args ftype (Obj.magic f)))
          | Coq_false -> Sskip), trest))
    | Sintro ->
      let push_temps = zero_stms (length (some_temps ftype (Obj.magic f))) in
      Some (app push_temps trest))

(** val allocate_locals :
    (ident, coq_type) prod list -> nat -> nat PTree.t **)

let rec allocate_locals ids base =
  match ids with
  | Coq_nil -> PTree.empty
  | Coq_cons (p, rest) ->
    let Coq_pair (id, _) = p in
    PTree.set id base (allocate_locals rest (S base))

(** val allocate_all_locals : (ident, coq_type) prod list -> nat PTree.t **)

let allocate_all_locals ids =
  allocate_locals ids O

(** val allocate_fn_locals : Language2.coq_function -> nat PTree.t **)

let allocate_fn_locals f =
  allocate_all_locals (all_temps ftype (Obj.magic f))

(** val stacked_function :
    Language2.coq_function -> Language0.coq_function option **)

let stacked_function f =
  bind (stacked_code (allocate_fn_locals f) f f.Language2.fn_code) (fun c ->
    Some c)

(** val stacked_fundef :
    Language2.coq_function -> Language0.coq_function option **)

let stacked_fundef =
  stacked_function

(** val stacked_fundefs :
    Language2.coq_function PTree.t -> Language0.coq_function PTree.t option **)

let stacked_fundefs t0 =
  transl_tree stacked_fundef t0

(** val stacked_methods :
    Language2.coq_function option IntMap.t -> Language0.coq_function option
    IntMap.t option **)

let stacked_methods methods =
  transl_map stacked_fundef methods

(** val stacked_constructor :
    Language2.coq_function option -> Language0.coq_function option **)

let stacked_constructor = function
| Some c -> bind (stacked_fundef c) (fun f -> Some f)
| None -> None

(** val stacked_genv : Language2.genv -> genv option **)

let stacked_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (stacked_fundefs fundefs) (fun fundefs0 ->
    bind (stacked_methods methoddefs) (fun methoddefs0 ->
      bind (stacked_constructor constructor) (fun constructor0 -> Some
        { Genv.genv_vars = vars; Genv.genv_funcs = funcs; Genv.genv_methods =
        methods; Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
        Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
        constructor0) })))

(** val stacked_program : Language2.program -> program option **)

let stacked_program = function
| Coq_pair (ge, body) ->
  bind (stacked_genv ge) (fun cge ->
    match label_functions cge.Genv.genv_fundefs with
    | Coq_true ->
      (match label_methods cge.Genv.genv_methoddefs with
       | Coq_true -> Some (Coq_pair (cge, body))
       | Coq_false -> None)
    | Coq_false -> None)

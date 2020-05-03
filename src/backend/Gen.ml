open AST
open BinInt
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language
open Maps0
open Options
open Trees

(** val int_t : coq_type **)

let int_t =
  Tint (I256, Unsigned)

(** val struct_lvalue : expr -> coq_type -> ident -> expr option **)

let struct_lvalue ex_l struct_type field =
  match struct_type with
  | Tstruct (_, fld) ->
    bind2 (struct_field fld field) (fun offset _ -> Some (Ebinop (Osha_2,
      ex_l, (Econst_int256 ((Int256.repr (Z.of_nat offset)), int_t)), int_t)))
  | _ -> None

(** val array_lvalue : expr -> expr -> expr option **)

let array_lvalue ex_l index =
  Some (Ebinop (Osha_2, ex_l, index, int_t))

(** val hash_lvalue : expr -> expr -> expr option **)

let hash_lvalue ex_l key =
  Some (Ebinop (Osha_2, ex_l, key, int_t))

(** val clike_rvalue : expr -> expr option **)

let rec clike_rvalue = function
| Econst_int (_, _) -> None
| Evar (id, ty) -> Some (Ederef ((Evar (id, ty)), ty))
| Ederef (_, _) -> None
| Eunop (op, ex0, ty) ->
  bind (clike_rvalue ex0) (fun rv -> Some (Eunop (op, rv, ty)))
| Ebinop (op, ex1, ex2, ty) ->
  bind (clike_rvalue ex1) (fun rv1 ->
    bind (clike_rvalue ex2) (fun rv2 -> Some (Ebinop (op, rv1, rv2, ty))))
| Efield (ex0, id, ty) ->
  bind (clike_lvalue ex0) (fun lv ->
    bind (struct_lvalue lv (typeof ex0) id) (fun ex_l -> Some (Ederef (ex_l,
      ty))))
| Earrayderef (ex1, ex2, ty) ->
  bind (clike_lvalue ex1) (fun lv ->
    bind (clike_rvalue ex2) (fun rv ->
      bind (array_lvalue lv rv) (fun ex_l -> Some (Ederef (ex_l, ty)))))
| Ehashderef (ex1, ex2, ty) ->
  bind (clike_lvalue ex1) (fun lv ->
    bind (clike_rvalue ex2) (fun rv ->
      bind (hash_lvalue lv rv) (fun ex_l -> Some (Ederef (ex_l, ty)))))
| Ecall1 (b, ex0, ty) ->
  bind (clike_rvalue ex0) (fun rv -> Some (Ecall1 (b, rv, ty)))
| x -> Some x

(** val clike_lvalue : expr -> expr option **)

and clike_lvalue = function
| Evar (id, ty) -> Some (Evar (id, ty))
| Efield (ex0, id, _) ->
  bind (clike_lvalue ex0) (fun lv -> struct_lvalue lv (typeof ex0) id)
| Earrayderef (ex1, ex2, _) ->
  bind (clike_lvalue ex1) (fun lv ->
    bind (clike_rvalue ex2) (fun rv -> array_lvalue lv rv))
| Ehashderef (ex1, ex2, _) ->
  bind (clike_lvalue ex1) (fun lv ->
    bind (clike_rvalue ex2) (fun rv -> hash_lvalue lv rv))
| _ -> None

(** val clike_rvalue_list : expr list -> expr list option **)

let rec clike_rvalue_list = function
| Coq_nil -> Some Coq_nil
| Coq_cons (hd, tl) ->
  bind (clike_rvalue hd) (fun first ->
    bind (clike_rvalue_list tl) (fun rest -> Some (Coq_cons (first, rest))))

(** val clike_optvalue : expr option -> expr option option **)

let clike_optvalue = function
| Some e -> bind (clike_rvalue e) (fun rv -> Some (Some rv))
| None -> None

(** val clike_stm : statement -> statement option **)

let rec clike_stm = function
| Sassign (lv, rv) ->
  bind (clike_lvalue lv) (fun lv' ->
    bind (clike_rvalue rv) (fun rv' -> Some (Sassign (lv', rv'))))
| Sset (id, rv) -> bind (clike_rvalue rv) (fun rv' -> Some (Sset (id, rv')))
| Scall (id, label, args) ->
  bind (clike_rvalue_list args) (fun rv_list -> Some (Scall (id, label,
    rv_list)))
| Ssequence (stm1, stm2) ->
  bind (clike_stm stm1) (fun seq1 ->
    bind (clike_stm stm2) (fun seq2 -> Some (Ssequence (seq1, seq2))))
| Sifthenelse (ex, stm1, stm2) ->
  bind (clike_rvalue ex) (fun ex0 ->
    bind (clike_stm stm1) (fun true_stm ->
      bind (clike_stm stm2) (fun false_stm -> Some (Sifthenelse (ex0,
        true_stm, false_stm)))))
| Sloop loop -> bind (clike_stm loop) (fun loop0 -> Some (Sloop loop0))
| Sreturn retval -> bind (clike_optvalue retval) (fun rv -> Some (Sreturn rv))
| Stransfer (addr, val0) ->
  bind (clike_rvalue addr) (fun addr0 ->
    bind (clike_rvalue val0) (fun val1 -> Some (Stransfer (addr0, val1))))
| Scallmethod (addr, retvals, funsig, val0, args) ->
  bind (clike_rvalue addr) (fun addr0 ->
    bind (clike_rvalue val0) (fun val1 ->
      bind (clike_rvalue_list args) (fun args0 -> Some (Scallmethod (addr0,
        retvals, funsig, val1, args0)))))
| Slog topic -> bind (clike_rvalue topic) (fun topic0 -> Some (Slog topic0))
| x -> Some x

(** val clike_function : coq_function -> coq_function option **)

let clike_function f =
  bind (clike_stm f.fn_body) (fun stm -> Some { fn_return = f.fn_return;
    fn_params = f.fn_params; fn_temps = f.fn_temps; fn_body = stm })

(** val clike_constructor :
    coq_function option -> coq_function option option **)

let clike_constructor = function
| Some f0 -> bind (clike_function f0) (fun f1 -> Some (Some f1))
| None -> Some None

(** val clike_functions :
    coq_function PTree.t -> coq_function PTree.t option **)

let clike_functions defs =
  transl_tree clike_function defs

(** val clike_methoddefs :
    coq_function option IntMap.t -> coq_function option IntMap.t option **)

let clike_methoddefs defs =
  transl_map clike_function defs

(** val clike_genv : genv -> genv option **)

let clike_genv ge =
  let vars = ge.Genv.genv_vars in
  let defs = ge.Genv.genv_defs in
  let names = ge.Genv.genv_funcs in
  let functions = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (clike_functions functions) (fun functions0 ->
    bind (clike_methoddefs methoddefs) (fun methoddefs0 ->
      bind (clike_constructor constructor) (fun constructor0 -> Some
        { Genv.genv_vars = vars; Genv.genv_funcs = names; Genv.genv_methods =
        sigs; Genv.genv_defs = defs; Genv.genv_fundefs = functions0;
        Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor =
        constructor0 })))

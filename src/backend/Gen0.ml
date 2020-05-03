open BinNums
open BinPos
open Coqlib
open Ctypes
open Datatypes
open Globalenvs
open Language1
open Language
open Maps0
open Options
open Specif
open Trees

type state = { st_nextnode : positive; st_code : code }

(** val st_nextnode : state -> positive **)

let st_nextnode x = x.st_nextnode

(** val st_code : state -> code **)

let st_code x = x.st_code

(** val init_state : state **)

let init_state =
  { st_nextnode = Coq_xH; st_code = PTree.empty }

type 'a res =
| Error
| OK of 'a * state

type 'a mon = state -> 'a res

(** val ret : 'a1 -> 'a1 mon **)

let ret x s =
  OK (x, s)

(** val error : 'a1 mon **)

let error _ =
  Error

(** val bind : 'a1 mon -> ('a1 -> 'a2 mon) -> 'a2 mon **)

let bind f g s =
  match f s with
  | Error -> Error
  | OK (a, s') -> g a s'

(** val add_instr : Language1.statement -> node mon **)

let add_instr i s =
  let n = s.st_nextnode in
  OK (n, { st_nextnode = (Pos.succ n); st_code = (PTree.set n i s.st_code) })

(** val reserve_instr : node mon **)

let reserve_instr s =
  let n = s.st_nextnode in
  OK (n, { st_nextnode = (Pos.succ n); st_code = s.st_code })

(** val check_empty_node : state -> node -> sumbool **)

let check_empty_node s n =
  match PTree.get n s.st_code with
  | Some _ -> Coq_right
  | None -> Coq_left

(** val update_instr : node -> Language1.statement -> coq_unit mon **)

let update_instr n i s =
  match plt n s.st_nextnode with
  | Coq_left ->
    (match check_empty_node s n with
     | Coq_left ->
       OK (Coq_tt, { st_nextnode = s.st_nextnode; st_code =
         (PTree.set n i s.st_code) })
     | Coq_right -> Error)
  | Coq_right -> Error

(** val cgraph_statement :
    statement -> node -> node -> node -> node option -> node mon **)

let rec cgraph_statement s nd nret nrev nbrk =
  match s with
  | Sskip -> ret nd
  | Sassign (lv, rv) -> add_instr (Language1.Sassign (lv, rv, nd))
  | Sset (id, rv) -> add_instr (Language1.Sset (id, rv, nd))
  | Scall (retval, lab, args) ->
    add_instr (Language1.Scall (retval, lab, args, nd))
  | Ssequence (s1, s2) ->
    bind (cgraph_statement s2 nd nret nrev nbrk) (fun ns ->
      cgraph_statement s1 ns nret nrev nbrk)
  | Sifthenelse (c, strue, sfalse) ->
    bind (cgraph_statement sfalse nd nret nrev nbrk) (fun nfalse ->
      bind (cgraph_statement strue nd nret nrev nbrk) (fun ntrue ->
        add_instr (Scond (c, ntrue, nfalse))))
  | Sloop sbody ->
    bind reserve_instr (fun n1 ->
      bind (cgraph_statement sbody n1 nret nrev (Some nd)) (fun n2 ->
        bind (update_instr n1 (Language1.Sskip n2)) (fun _ -> ret n1)))
  | Sbreak -> (match nbrk with
               | Some nbrk0 -> ret nbrk0
               | None -> error)
  | Sreturn retval -> add_instr (Language1.Sreturn (retval, nret))
  | Stransfer (a, v) -> add_instr (Language1.Stransfer (a, v, nrev, nd))
  | Scallmethod (a, rvs, sig0, v, args) ->
    add_instr (Language1.Scallmethod (a, rvs, sig0, v, args, nrev, nd))
  | Slog l -> add_instr (Language1.Slog (l, nd))
  | Srevert -> ret nrev

(** val cgraph_function : coq_function -> Language1.coq_function option **)

let cgraph_function f =
  let cgraph_fun =
    bind (add_instr Sdone) (fun nret ->
      bind (add_instr Language1.Srevert) (fun nrev ->
        cgraph_statement f.fn_body nret nret nrev None))
  in
  (match cgraph_fun init_state with
   | Error -> None
   | OK (nentry, s) ->
     Some { Language1.fn_return = f.fn_return; Language1.fn_params =
       f.fn_params; Language1.fn_temps = f.fn_temps; fn_code = s.st_code;
       fn_entrypoint = nentry })

(** val empty_constructor : coq_function **)

let empty_constructor =
  { fn_return = Tvoid; fn_params = Coq_nil; fn_temps = Coq_nil; fn_body =
    Sskip }

(** val cgraph_constructor :
    coq_function option -> Language1.coq_function option option **)

let cgraph_constructor = function
| Some f0 -> Options.bind (cgraph_function f0) (fun cf -> Some (Some cf))
| None ->
  Options.bind (cgraph_function empty_constructor) (fun cf -> Some (Some cf))

(** val cgraph_functions :
    coq_function PTree.t -> Language1.coq_function PTree.t option **)

let cgraph_functions defs =
  transl_tree cgraph_function defs

(** val cgraph_methoddefs :
    coq_function option IntMap.t -> Language1.coq_function option IntMap.t
    option **)

let cgraph_methoddefs defs =
  transl_map cgraph_function defs

(** val cgraph_genv : genv -> Language1.genv option **)

let cgraph_genv ge =
  let vars = ge.Genv.genv_vars in
  let names = ge.Genv.genv_funcs in
  let fundefs = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  Options.bind (cgraph_functions fundefs) (fun fundefs0 ->
    Options.bind (cgraph_methoddefs methoddefs) (fun methoddefs0 ->
      Options.bind (cgraph_constructor constructor) (fun constructor0 -> Some
        { Genv.genv_vars = vars; Genv.genv_funcs = names; Genv.genv_methods =
        sigs; Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
        Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor =
        constructor0 })))

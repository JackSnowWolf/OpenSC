open Ast
open Sast
let sprintf = Printf.sprintf

open Language   



let rec positive_of_int n =
  let open BinNums in
  if n = 1 then
    Coq_xH
  else if (n land 1) = 1 then
    Coq_xI (positive_of_int (n asr 1))
  else
    Coq_xO (positive_of_int (n asr 1))
  
let coq_Z_of_int n =
  let open BinNums in
  if n = 0 then Z0
  else if n > 0 then Zpos (positive_of_int n)
  else Zneg (positive_of_int (-n))

(* let rec coqlist_of_list = function
| [] -> Datatypes.Coq_nil
| x::xs -> Datatypes.(Coq_cons (x, coqlist_of_list xs)) *)


let rec int_of_positive p =
let open BinNums in
match p with
| Coq_xI rest -> 2*(int_of_positive rest) + 1
| Coq_xO rest -> 2*(int_of_positive rest)
| Coq_xH -> 1


let int_of_z =
let open BinNums in
function
| Z0 -> 0
| Zpos rest -> int_of_positive rest
| Zneg rest -> -(int_of_positive rest)
       
(* end of functions to move. *)	

(* id generator!!!  may need to revised  *)
let ident_table : (string, int) Hashtbl.t = Hashtbl.create 1000
let ident_counter : int ref = ref 550

let ident_generator = fun prefix midfix postfix ->
    let id = (prefix ^ midfix ^ "_"^ postfix) in
    try positive_of_int (Hashtbl.find ident_table id)
    with Not_found -> begin
       let n = !ident_counter in
       ident_counter := !ident_counter + 1;
       Hashtbl.add ident_table id n;
       positive_of_int n
      end

let struct_name_to_ident2 = ident_generator "" "struct"
let struct_field_name_to_ident2 = ident_generator "" "field"
let backend_ident_of_globvar  = ident_generator "var_" "var2"
let backend_ident_of_funcname = ident_generator "ident_" "function"
let backend_ident_of_tempvar i = positive_of_int i

let rec gen_ctype =
  let open Ctypes in 
  function 
  | Int -> Tint (I256, Unsigned)
  | Uint x  -> Tint (I256, Unsigned)
  | Void x -> Tvoid
  (* | Mapstruct (t1, t2) -> Thashmap (gen_ctype t1, gen_ctype t2) *)
  (* and gen_ctype_fields
  do we need ctype fields??
  leave for now
  *)

  let gen_unop = 
  let open Cop in
  function
  | Neq -> Oneg
  (* | OPnot -> Onotbool
  | OPbitnot -> Onotint
  | OPbitneg -> Onotint
  | OPsha_1 -> Osha_1  may need to suppport more unary op *)

let gen_binop =
  let open Cop in
  function
  | Add -> Oadd
  | Sub -> Osub
  | Times -> Omul
  | Divide -> Odiv
  | And -> Oand
  | Or -> Oor
  | Equal -> Oeq
  | Neq -> One
  | RGT -> Olt
  | RGTEQ -> Ole
  | LGT -> Ogt
  | LGTEQ -> Oge
(*   | OPlt -> Olt
  | OPle -> Ole
  | OPgt -> Ogt
  | OPge -> Oge
  | OPshl -> Oshl
  | OPshr -> Oshr
  | OPxor -> Oxor
  | OPbitand -> Oand
  | OPbitor -> Oor
  | OPsha_2 -> Osha_2 *)

let rec gen_rexpr e =
  let open Ctypes in
  let open Integers in
  let open Language in
  let open MachineModel in
  match e with
  | (t, SNumLit l) -> Econst_int256 (Int256.repr (coq_Z_of_int l), gen_ctype Int)
  | (t, SBoolLit l) -> (match l with 
                        |true -> Econst_int256 (Int256.one, Tint (I256, Unsigned))
                        |false -> Econst_int256 (Int256.zero, Tint (I256, Unsigned)) )
  (* SStrLit *)
  | (t, SId l) -> Evar (backend_ident_of_globvar l, gen_ctype t)
  | (t1, SBinop ((t2, se1), op, (t3, se2))) -> Ebinop (gen_binop op, gen_rexpr (t2, se1), gen_rexpr (t3, se2), gen_ctype t1)	
  | (t, SId l)-> Etempvar (backend_ident_of_tempvar 1, gen_ctype t) (* this is wrong leave for now *)
  | (t, SEnvLit(s1, s2)) -> (match s2 with |"sender" -> Ecall0 (Baddress, Tvoid))

(* need to revised! *)
let rec gen_lexpr e =
  let open Integers in
  let open Language in
  match e with
  |(t, SId l) -> Evar (backend_ident_of_globvar l, gen_ctype t)


(* generate specifc statement *)
let gen_assign_stmt e1 e2 = 
  let open Language in
  Sassign(gen_rexpr e1, gen_rexpr e2)

let gen_set_stmt id e1 =
  let open Language in
  Sset (positive_of_int id, gen_rexpr e1)

(* let rec gen_nonempty_params (e, t) =
  let open Datatypes in
  match ts with
  | t :: ts -> Coq_pair (positive_of_int (1), gen_ctype t) :: gen_nonempty_params (base+1) ts
  |  _ -> [] *)

(* let gen_params (e, t) =
  match e with
  | [] -> []
  | params -> gen_nonempty_params 1 e *)

let rec gen_tempenv = function
  | [] -> []
  | (id, typ) :: ts -> Datatypes.Coq_pair (backend_ident_of_tempvar id, gen_ctype typ) :: gen_tempenv ts

       
let builtinBase_local_ident_start = 10

(* need a big change..... need obj name*)
let gen_methoddef m =
  let open Datatypes in
  let dest = builtinBase_local_ident_start in  
  (* let is_pure, has_return = method_classify mt in *)
  let body = gen_set_stmt  builtinBase_local_ident_start (List.hd m.sstorage_body) in
  let ret_type = (gen_ctype m.sreturns)in
  { fn_return = ret_type ;
    fn_params = Coq_nil;
    fn_temps  = Coq_nil; (* coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype) :: gen_cmd_locals m.aMethodBody dest))*)
    fn_body =  (* (if has_return then
                  Ssequence (body,
			     (Sreturn Tvoid))
		else *)
		  body (* ) *)
  }

(* leave for now
let method_classify mt =
  (* is pure *) mt.sreturns = True,
  (* has return *) mt.aMethodReturnType.aTypeDesc <> ATbuiltin Tunit	
*)




(* 
open AST (* type ident = positive -> BinNums: type positive = | Coq_xI of positive | Coq_xO of positive | Coq_xH *)
open Cop (* unary_operation, binary_operation *)
open Ctypes (* type coq_type = Tvoid | ... | ... *)
open Datatypes (* type ('a, 'b) prod = Coq_pair of 'a * 'b *)
open Globalenvs (* Genv: type ('f, 'v) t  *)
open Integers
open MachineModel (* builtin0, builtin1 (EVM built-in) *)

type genv = { 
    genv_vars : ident list; 
    genv_funcs : ident list;
    genv_methods : Int.int list; 
    genv_defs : coq_type PTree.t;
    genv_fundefs : coq_function PTree.t;
    genv_methoddefs : coq_function option IntMap.t;
    genv_constructor : coq_function option 
  }

*)
(* function_selector_intval_of_method: this one exists in abi.ml ........ *)
let make_methname m = coq_Z_of_int (1)

let rec coqlist_of_list =
  let open Datatypes in
  function
  | [] -> Coq_nil
  | x::xs -> (Coq_cons (x, coqlist_of_list xs)) 

(* let rec gen_identlist =
  let open AST in 
  function
  | [] -> []
  | x :: xs -> (positive_of_int id) :: (gen_identlist xs)*)
  
(* let rec filter_map f ls =
  match ls with
  | [] -> []
  | x::xs -> match f x with
        | Some y -> y :: filter_map f xs
        | None -> filter_map f xs *)

(* let gen_object_methods gen_methodname gen_method o =
  let open Datatypes in
  coqlist_of_list
    (filter_map
    (fun m ->
    Some (Coq_pair (gen_methodname m,
        gen_method o))
    ) o ) *)


(* let rec add_genv_methods methods ge =
  let open Globalenvs in
  match methods with
  | Coq_nil -> ge
  | Coq_cons (p, rest) ->
    let Coq_pair (sig0, fundef) = p in
    let r = add_genv_methods rest ge in
    { genv_vars = r.genv_vars; genv_funcs = r.genv_funcs; genv_methods =
    (Coq_cons (sig0, r.genv_methods)); genv_defs = r.genv_defs;
    genv_fundefs = r.genv_fundefs; genv_methoddefs =
    (IntMap.set sig0 (Some fundef) r.genv_methoddefs); genv_constructor =
    r.genv_constructor } *)


(* let translatetoMinic (sinterface, simplementation) = 
  let open Datatypes in
  let open Language in
  let open Globalenvs in 
  let open Maps0 in
    (* let global_var = (backend_ident_of_globvar "storageData") *)

  let genv = {
    Genv.genv_vars = (coqlist_of_list []); 
    Genv.genv_funcs = (coqlist_of_list []) ;
    Genv.genv_methods = (coqlist_of_list []); 
    Genv.genv_defs = PTree.empty;
    Genv.genv_fundefs = PTree.empty;
    (* (IntMap.init None) *)
    Genv.genv_methoddefs = add_genv_methods (Int.int, gen_object_methods(make_methname 1, (gen_methoddef), (List.hd simplementation.smethods))) ; 
    Genv.genv_constructor = None;
  }
  in genv *)



(* Print MiniC expressions/statements, for debugging purposes. *)
let rec string_of_ctype = 
  let open Ctypes in
  function
  | Tvoid -> "Tvoid"
  | Tint (_,_) -> "TInt (SIZE,SIGNEDNESS)"
  | Tpointer _ -> "Tpointer"
  | Tarray (t,z) -> ("Tarray ("^string_of_ctype t^","^string_of_int(int_of_z z)^")")
  | Thashmap (t1,t2) -> ("Thashmap("^string_of_ctype t1^","^string_of_ctype t2^")")
  | Tfunction (ts,t) -> "Tfunction (TYPES,TYPE)"
  | Tstruct (id,flds) -> "Tstring (IDENT, FIELDS)"
  | Tunion  (id,flds) -> "Tunion (IDENT, FIELDS)"
  | Tcomp_ptr id -> "Tcomp_ptr ID"


let rec string_of_expr = function
  | Econst_int (z, t) -> ("Econst_int (" ^ string_of_int (int_of_z z) ^ ","^string_of_ctype t^")")
  | Econst_int256 (z, t) -> ("Econst_int (" ^ string_of_int (int_of_z z) ^ ","^string_of_ctype t^")")
  | Evar (id,t) -> ("Evar("^string_of_int (int_of_positive id)^","^string_of_ctype t^")")
  | Etempvar (id,t) -> ("Etempvar("^string_of_int (int_of_positive id)^","^string_of_ctype t^")")
  | Ederef (e,t) -> ("Ederef(" ^ string_of_expr e ^","^ string_of_ctype t ^")")
  | Eunop (op,e,t) -> ("Eunop(OP,"^string_of_expr e^","^string_of_ctype t ^")")
  | Ebinop (op,e1,e2,t) -> ("Ebinop(OP,"^string_of_expr e1^","^string_of_expr e2^","^string_of_ctype t ^")")
  | Efield (e, ident, t) ->("Efield("^string_of_expr e^","^string_of_int (int_of_positive ident)^","^string_of_ctype t^")")
  | Earrayderef (e1,e2,t) -> ("Earrayderef("^string_of_expr e1 ^","^string_of_expr e2^","^string_of_ctype t^")")
  | Ehashderef (e1,e2,t) -> ("Ehashderef("^string_of_expr e1 ^","^string_of_expr e2^","^string_of_ctype t^")")
  | Ecall0 (bt,t) -> "Ecall0(BUILTIN,TYPE)"
  | Ecall1 (bt,e,t) -> "Ecall0(BUILTIN,EXPR,TYPE)"

let rec string_of_params = 
  let open Datatypes in
  function
  | Coq_cons (Coq_pair(id, t) , params) -> "("^string_of_int (int_of_positive id) ^","^string_of_ctype t ^")::"^ string_of_params params
  | Coq_nil -> "nil"

(* 
let gen_methoddef m =
  let open Datatypes in
  let dest = builtinBase_local_ident_start in  
  (* let is_pure, has_return = method_classify mt in *)
  let body = gen_set_stmt  builtinBase_local_ident_start (List.hd m.sstorage_body) in
  let ret_type = (gen_ctype m.sreturns)in
  { fn_return = ret_type ;
    fn_params = Coq_nil;
    fn_temps  = Coq_nil; (* coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype) :: gen_cmd_locals m.aMethodBody dest))*)
    fn_body =  (* (if has_return then
                  Ssequence (body,
            (Sreturn Tvoid))
    else *)
      body (* ) *)
  }

let gen_methoddef objname m =
  let open Backend.Datatypes in
  let mt = m.aMethodType in
  let dest = builtinBase_local_ident_start in  
  let is_pure, has_return = method_classify mt in
  let body = gen_cmd objname is_pure m.aMethodBody dest in
  let ret_type = gen_ctype mt.aMethodReturnType.aTypeCtype in
  { fn_return = ret_type ;
    fn_params = coqlist_of_list (gen_params builtinBase_local_ident_start mt.aMethodArgumentTypes);
    fn_temps  = coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype)
                :: gen_cmd_locals m.aMethodBody dest));
    fn_body =  (if has_return then
                  Ssequence (body,
            (Sreturn (Some (Etempvar (positive_of_int dest,
                    ret_type)))))
    else
      body)
  } *)

let gen_object_methods gen_methodname gen_method o =
  let open Backend.Datatypes in
  coqlist_of_list
    (List.map
        (fun m -> Some (Coq_pair (gen_methodname m, gen_method o.sconsturctor_def.sname m)))
        o.smethods) 

let gen_object o =
  let open Backend.Datatypes in
  let open Backend.Globalenvs.Genv in
  (* let make_funcname m = backend_ident_of_funcname o.sconsturctor_def.sname m.smethodname in *)
  let make_methname m = coq_Z_of_int (function_selector_intval_of_method m) in
  new_genv (gen_object_fields o) Coq_nil
      (gen_object_methods make_methname gen_methoddef o)
      None  

let minicgen (sinterface, simplementation) = gen_object simplementation

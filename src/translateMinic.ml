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

let rec coqlist_of_list =
  let open Datatypes in
  function
  | [] -> Coq_nil
  | x::xs -> (Coq_cons (x, coqlist_of_list xs)) 
       
let rec filter_map f ls =
  let open Datatypes in
    match ls with
    | [] -> []
    | x::xs -> match f x with
          | Some y -> y :: filter_map f xs
          | None -> filter_map f xs

let ident_table : (string, int) Hashtbl.t = Hashtbl.create 1000
let ident_counter : int ref = ref 550

(** ident_generator : positive **)
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
let backend_ident_of_tempvar =  ident_generator "temp_" "var"

let rec gen_ctype =
  let open Ctypes in 
  function 
  | Bool -> Tint (I256, Unsigned)
  | Int -> Tint (I256, Signed)
  | Uint x  -> Tint (I256, Unsigned)
  | Void x -> Tvoid
  | Address x -> Tint (I256, Unsigned)
  | Mapstruct (key_ty, val_ty) -> Thashmap (gen_ctype (List.hd key_ty), gen_ctype val_ty)

  (* | Mapstruct (t1, t2) -> Thashmap (gen_ctype t1, gen_ctype t2) *)
  (* and gen_ctype_fields
  do we need ctype fields??
  leave for now
  *)

let gen_unop = 
  let open Cop in
  function
  | Neq -> Oneg
  | _ -> raise (Failure "Not a unop!")
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
  | PASSIGN -> raise (Failure "PASSIGN should be solved as Storageassign in expr")

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
  let open Integers in
  let open Language in
  match e with
  | (t, SId(Sglobal,l)) -> Evar (backend_ident_of_globvar l, gen_ctype t)
  | (t, SId(Slocal,l)) -> Etempvar (backend_ident_of_tempvar l, gen_ctype t)
  | se -> raise (Failure ("Not implemented: " ^ string_of_sexpr se))

let rec gen_lexpr e =
  let open Ctypes in
  let open Integers in
  let open Language in
  let open MachineModel in
  match e with
  | (t, SNumLit l) -> Econst_int256 (Int256.repr (coq_Z_of_int l), gen_ctype Int)
  | (t, SBoolLit l) -> (match l with 
                        |true -> Econst_int256 (Int256.one, Tint (I256, Unsigned))
                        |false -> Econst_int256 (Int256.zero, Tint (I256, Unsigned)) )
  | (t, SId(Sglobal,l)) -> Evar (backend_ident_of_globvar l, gen_ctype t)
  | (t, SId(Slocal,l)) -> Etempvar (backend_ident_of_tempvar l, gen_ctype t)
  | (t1, SBinop ((t2, se1), op, (t3, se2))) -> Ebinop (gen_binop op, gen_lexpr (t2, se1), gen_lexpr (t3, se2), gen_ctype t1)	
  | (t, SComparsion ((t1, se1), op, (t2, se2))) -> Ebinop (gen_binop op, gen_lexpr (t1, se1), gen_lexpr (t2, se2), gen_ctype t)	
  | (t, SMapexpr((t1, se1), selist)) -> 
    (* TODO: convert selist's type to Tstruct *)
    let se2 = List.hd selist in
    Ehashderef(gen_lexpr (t1, se1), gen_lexpr se2, gen_ctype t)
  | (t, SEnvLit(s1, s2)) -> 
    (
      match s2 with 
      | "sender" -> Ecall0 (Bcaller, Tvoid)
      | "value" -> Ecall0 (Bcallvalue, Tvoid)
      | "origin" -> Ecall0 (Borigin, Tvoid)
      | "sig" -> Ecall0 (Baddress, Tvoid)
      | "data" -> Ecall0 (Baddress, Tvoid)
      | _ -> let _ = print_endline ("Waring: Env key may not support") in
      Ecall0 (Baddress, Tvoid)
    )
  | se -> raise (Failure ("Not implemented: " ^ string_of_sexpr se))

(** gen_assign_stmt : statement **)
let gen_assign_stmt e1 e2 = 
  let open Language in
  Sassign(gen_lexpr e1, gen_lexpr e2)

let gen_set_stmt id e1 =
  let open Language in
  Sset (positive_of_int id, gen_rexpr e1)

let gen_guard_stmt e = 
  let open Language in
  Sifthenelse(gen_lexpr e, Sskip, Srevert)

(* sparams: decls list *)
(** gen_params : 
    (ident, coq_type) prod list; **)
let gen_params sparams =
  let open Datatypes in
  let open Globalenvs.Genv in
  let cvt = function
    | Var(Id str, typ) -> Some (Coq_pair(backend_ident_of_tempvar str, gen_ctype typ))
    | _ -> None
  in
  coqlist_of_list (filter_map cvt sparams)

(* let rec gen_nonempty_params (e, t) =
  let open Datatypes in
  match ts with
  | t :: ts -> Coq_pair (positive_of_int (1), gen_ctype t) :: gen_nonempty_params (base+1) ts
  |  _ -> [] *)

(* storagebody: sexpr list *)
(** gen_storage_cmd : statement **)
let gen_storage_cmd storebody =
  let open Datatypes in
  let rec list2seq = function 
    | [] -> Sskip
    | hd::[] -> hd
    | hd::tl -> Ssequence(hd, list2seq tl)
  in
  let sexpr2Sassign = function
    | (typ, SStorageassign(lsexpr, rsexpr)) -> Some(gen_assign_stmt lsexpr rsexpr) (* (gen_assign_stmt (Int, (SId "storedData")) (Int, (SNumLit 12))) *)
    | _ -> None
  in
  list2seq (filter_map sexpr2Sassign storebody)

let gen_guard_cmd guardbody = 
  let open Datatypes in
  let rec list2seq = function 
    | [] -> Sskip
    | hd::[] -> hd
    | hd::tl -> Ssequence(hd, list2seq tl)
  in
  let sexpr2stmt = function
    | se -> Some(gen_guard_stmt se)
  in
  list2seq (filter_map sexpr2stmt guardbody)

let gen_return_cmd (return_type, sx) =
  let open Datatypes in
  let return_expr =
    match return_type with
    Void(_) -> None
    | _ -> Some(gen_lexpr (return_type, sx)) 
  in 
  Sreturn(return_expr)

(** gen_methoddef : coq_function **)
let gen_methoddef m =
  let open Datatypes in
  (* let dest = builtinBase_local_ident_start in   *) (* let builtinBase_local_ident_start = 10 *)
  (* let is_pure, has_return = method_classify mt in *)
  (* let body = gen_set_stmt  builtinBase_local_ident_start (List.hd m.sstorage_body) in *)
  let body = 
    Ssequence(Ssequence(gen_guard_cmd m.sguard_body, gen_storage_cmd m.sstorage_body), gen_return_cmd m.sreturns)
  in
  let ret_type (ty, sx) = gen_ctype ty in
  { 
    fn_return = ret_type m.sreturns;
    fn_params = gen_params m.sparams; (* (ident, coq_type) prod list; *)
    fn_temps  = Coq_nil; (* coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype) :: gen_cmd_locals m.aMethodBody dest))*)
    fn_body =  (* (if has_return then
                  Ssequence (body,
			     (Sreturn Tvoid))
		else *)
      body
      (* Ssequence (body,
        (Sreturn (Some (Etempvar (positive_of_int 10,
             ret_type))))) *)
  }

(* let gen_methoddef objname m =
  let open Datatypes in
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

(* leave for now
let method_classify mt =
  (* is pure *) mt.sreturns = True,
  (* has return *) mt.aMethodReturnType.aTypeDesc <> ATbuiltin Tunit	
*)

(** gen_object_methods : 
    (Int.int, coq_fun) prod list **)
let gen_object_methods gen_methodname gen_method o =
  let open Datatypes in
  coqlist_of_list
    (List.map
      (fun m -> Coq_pair (gen_methodname m, gen_method m))
      o.smethods) 

(** gen_object_fields :
    vars: (ident, coq_type) prod list **)
let gen_object_fields declist = 
  let open Datatypes in
  let open Globalenvs.Genv in
  let decl2gvars = function
    | TypeAssigndecl(Id s, t) -> Some (Coq_pair(backend_ident_of_globvar s, gen_ctype t))
    | MapAssigndecl(Id s, t) -> Some (Coq_pair(backend_ident_of_globvar s, gen_ctype t))
    | _ -> None
  in
  coqlist_of_list (filter_map decl2gvars declist)

(** gen_object : genv **)
(* (i, o) = (sinterface, simplementation) *)
let gen_object (i, o) =
  let open Datatypes in
  let open Globalenvs.Genv in
  let open Cryptokit in
  let keccak_intval (_, SStrLit str) =
    let hashval = hash_string (Hash.keccak 256) str in
      (0x01000000) * Char.code (String.get hashval 0)
    + (0x00010000) * Char.code (String.get hashval 1)
    + (0x00000100) * Char.code (String.get hashval 2)
    +                Char.code (String.get hashval 3) 
  in
  (* let make_funcname m = backend_ident_of_funcname o.sconsturctor_def.sname m.smethodname in *)
  (* let make_methname m = coq_Z_of_int 1101101111 in *)
  (* let make_methname m = coq_Z_of_int (function_selector_intval_of_method m) in *) (* function_selector_intval_of_method: from abi.ml *)
  let make_methname m = coq_Z_of_int (keccak_intval m.smethodname) in 
    new_genv (* new_genv: vars -> funcs -> methods -> constructor  *)
      (gen_object_fields i.sinterfacebody) (* vars: (ident, coq_type) prod list *)
      Coq_nil (* funcs: (id, coq_fun) prod list. Only the lower layers have funcs *)
      (gen_object_methods make_methname gen_methoddef o) (* methods: (Int.int, coq_fun) prod list *)
      None

let minicgen sprogram = gen_object sprogram

(** 
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
**)

(* 
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
  | Coq_nil -> "nil" *)

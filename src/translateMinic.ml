open Ast
open Sast
let sprintf = Printf.sprintf

open Language   

(* TODO: Need to revised *)

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

let rec coqlist_of_list = function
| [] -> Datatypes.Coq_nil
| x::xs -> Datatypes.(Coq_cons (x, coqlist_of_list xs))


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
  | Uint x  -> Tint (I256, Unsigned)
  | Void x -> Tvoid
  (*   | PointAssign ct -> coqgen_fatal_error __LOC__ "gen_ctype" "ACtpointer not supported"*)  
  (*   | ACtarray (n, ct) -> Tarray (gen_ctype ct, coq_Z_of_int n)*)  
(*   | MapAssign (e, t1, t2) -> Thashmap (gen_ctype t1, gen_ctype t2)
 *)  (* struct is not supported in current OpenSC *)
  (*     and gen_ctype_fields sname  =
  let open Backend.Ctypes in 
  function
  | [] -> Fnil
  | (fld,ct)::flds -> Fcons (struct_field_name_to_ident2 sname fld,
           gen_ctype ct,
           gen_ctype_fields sname flds)
      *)
  let gen_unop = 
  let open Cop in
  function
  | Neq -> Oneg
(*   | OPnot -> Onotbool
  | OPbitnot -> Onotint
  | OPbitneg -> Onotint
  | OPsha_1 -> Osha_1 *)

let gen_binop =
  let open Cop in
  function
  | Add -> Oadd
  | Sub -> Osub
  | Times -> Omul
  | Divide -> Oand
  | Or -> Oor
  | Equal -> Oeq
  | Neq -> One
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

  let gen_expr  = 
  let open Language in
  let open Sast in
  function
  | StypeAssign (se, t) -> Etempvar (backend_ident_of_globvar (string_of_sexpr se), gen_ctype t)


(* let rec gen_rexpr e =
  let open Ctypes in
  let open Integers in
  let open Language in
  match e.aRexprDesc with      
  | AEconst (CONint n) ->
     Econst_int256 (Int256.repr (coq_Z_of_int n), gen_ctype e.aRexprType.aTypeCtype)
  | AEconst (CONuint n) ->
     Econst_int256 (Int256.repr (coq_Z_of_int n), gen_ctype e.aRexprType.aTypeCtype)
  | AEconst (CONbool true) ->
    Econst_int256 (Int256.one, Tint (I256, Unsigned))
  | AEconst (CONbool false) ->
    Econst_int256 (Int256.zero, Tint (I256, Unsigned))
  | AEconst CONunit ->
    Econst_int256 (Int256.zero, Tvoid)
  | AEconst CONglobalpointer_undef ->
     coqgen_fatal_error __LOC__ "output_rexpr" "Internal error."
  | AEconstr_val (c, []) -> 
    Econst_int256 (begin match c.aTypeConstrImpl with
      | None | Some { aImplDesc = ACdefault } -> Int256.zero
      | Some { aImplDesc = ACint n }          -> (Int256.repr (coq_Z_of_int n))
      | _ -> coqgen_fatal_error __LOC__ "output_rexpr" "Internal error."
        end,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEconstr_val (c, _) -> coqgen_fatal_error __LOC__ "output_rexpr" "Internal error: Nonempty AEconstr is not supported."
  | AEtemp (n, _i) ->
    Etempvar (backend_ident_of_tempvar n, gen_ctype e.aRexprType.aTypeCtype)
  | AEunop (op, e') ->
     Eunop (gen_unop op,
      gen_rexpr e',
      gen_ctype e.aRexprType.aTypeCtype)
  | AEbinop (op, e1, e2) ->
     Ebinop (gen_binop op,
       gen_rexpr e1,
       gen_rexpr e2,
       gen_ctype e.aRexprType.aTypeCtype)      
  | AEbuiltin ("address",[]) ->
     Ecall0 (Backend.MachineModel.Baddress,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("origin",[]) ->
     Ecall0 (Backend.MachineModel.Borigin,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("caller",[]) ->
     Ecall0 (Backend.MachineModel.Bcaller,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("callvalue",[]) ->
     Ecall0 (Backend.MachineModel.Bcallvalue,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("coinbase",[]) ->
     Ecall0 (Backend.MachineModel.Bcoinbase,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("timestamp",[]) ->
     Ecall0 (Backend.MachineModel.Btimestamp,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("number",[]) ->
     Ecall0 (Backend.MachineModel.Bnumber,
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("balance",[e1]) ->
     Ecall1 (Backend.MachineModel.Bbalance, (gen_rexpr e1),
       gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("blockhash",[e1]) ->
     Ecall1 (Backend.MachineModel.Bblockhash, (gen_rexpr e1),
       gen_ctype e.aRexprType.aTypeCtype)
      
  | AEbuiltin (other,_) -> coqgen_fatal_error __LOC__ "output_rexpr"
               ("Internal error, encountered unknown builtin \""^other^"\".")

let rec gen_lexpr obj e =
  let open Backend.Integers in
  let open Backend.Language in
  match e.aLexprDesc with
  | AEvar i ->
     Evar (backend_ident_of_globvar obj i,
     gen_ctype e.aLexprType.aTypeCtype)
  | AEfield (e', f) ->
     Efield (gen_lexpr obj e',
             begin match e'.aLexprType.aTypeDesc with
       | ATdata (i, _) -> struct_field_name_to_ident2 i f
       | ATprod _ as d -> struct_field_name_to_ident2 ("struct_" ^ a_type_desc_to_ident d) f
       | _ -> coqgen_fatal_error __LOC__ "output_lexpr"
               "Only ATdata and ATprod can be accessed through structure field selector"
       end,
       gen_ctype e.aLexprType.aTypeCtype)
  | AEindex (e', idx) ->
    Earrayderef (gen_lexpr obj e', gen_rexpr idx, gen_ctype e.aLexprType.aTypeCtype)
  | AEhash (e', idx) ->
    Ehashderef (gen_lexpr obj e',  gen_rexpr idx, gen_ctype e.aLexprType.aTypeCtype)
 *)

(* END before gen_constr_assignments line 467 *)


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



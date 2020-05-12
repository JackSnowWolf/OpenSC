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
  | Mapstruct (t1, t2) -> Thashmap(gen_ctype t1, gen_ctype t2)

let gen_unop = 
  let open Cop in
  function
  | Neq -> Oneg


let gen_binop =
  let open Cop in
  function
  | Add -> Oadd
  | Sub -> Osub
  | Times -> Omul
  | Divide -> Odiv
  | Or -> Oor
  | Equal -> Oeq
  (*| Neq -> One*)
  | And -> Oand 
  | Or -> Oor 

(* type * sexpr *)
let rec gen_rexpr e =
  let open Ctypes in 
  let open Integers in
  let open Language in 
  match e with
  | SNumLit(x) -> Econst_int256 (Int256.repr (coq_Z_of_int x), gen_ctype e)
  | SBoolLit(false) -> Econst_int256 (Int256.zero, Tint (I256, Unsigned))
  | SBoolLit(true) -> Econst_int256 (Int256.one, Tint (I256, Unsigned))
  | SBinop(e1, op, e2) -> Ebinop (gen_binop op, gen_rexpr e1, gen_rexpr e2, gen_ctype e)
  | SEnvLit ("Env", "sender")-> Ecall0 (Backend.MachineModel.Baddress, gen_ctype e)
(* 
let rec gen_lexpr obj e =
  let open Backend.Integers in
  let open Backend.Language in
  | *)


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






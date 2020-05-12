open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* 
let strore_ids ta = function *)


(* need to implement *)
let check (signature, implementation) =

  (* Add variable id in interface to symbol table *)
  let add_var map var =
    let dup_err v = "duplicate variable " ^ (string_of_expr v) ^ " in interface"
    and make_err er = raise (Failure er)
    in match var with (* No duplicate variables or redefinitions of built-ins *)
      Var(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | Var(x, t) ->  StringMap.add (string_of_expr x) var map
    | TypeAssigndecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | TypeAssigndecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | MapAssigndecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | MapAssigndecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | Eventdecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | Eventdecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | _ -> map
  in

  (* Collect all variable names into one symbol table *)
  let var_decls = List.fold_left add_var StringMap.empty signature.interfacebody in

  (* Add method name in interface to symbol table *)
  let add_func map func =
    let dup_err v = "duplicate method " ^ (string_of_expr v) ^ " in interface"
    and make_err er = raise (Failure er)
    in match func with (* No duplicate variables or redefinitions of built-ins *)
      Constructordecl(l, t1, t2) when StringMap.mem (string_of_expr l) map -> make_err (dup_err l)
    | Constructordecl(l, t1, t2) ->  StringMap.add (string_of_expr l) func map
    | Methodecls (l, t1, t2) when StringMap.mem (string_of_expr l) map -> make_err (dup_err l)
    | Methodecls (l, t1, t2) ->  StringMap.add (string_of_expr l) func map
    | _ -> map
  in

  (* Collect all function names into one symbol table *)
  let func_decls = List.fold_left add_func StringMap.empty signature.interfacebody in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s func_decls
    with Not_found -> raise (Failure ("unrecognized method " ^ s))
  in

  let count_constructor num func =
    match func with
      Constructordecl(l, t1, t2) -> num +1
      | _ -> num
  in

  (* check constructor only announce once in interface *)
  let _  = 
    let constructor_num = List.fold_left count_constructor 0 signature.interfacebody in
    match constructor_num with
    0 -> raise (Failure "No constructor in interface")
    | 1 -> constructor_num
    | _ -> raise (Failure "Multiple constructors in interface")
  in
  

  let check_expr = function
    | NumLit l -> (Int, SNumLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StrLit l -> (Void("void"), SStrLit l)
    (* check Id retrun with the correct type, keep Int for now *)
    | Id x -> (Int, SId x)
    | EnvLit(x, y) -> (Int, SEnvLit(x,y))
    | Mapexpr(e1, e2) -> (Int, SMapexpr(e1, e2))
    | Binop(e1, op, e2) -> (Int, SBinop(e1, op, e2))
    | Logexpr(e1, e2) -> (Int, SLogexpr(e1, e2))
  in

  (* let check_decl = function
    | Var(expr,typ) -> 
    | TypeAssigndecl(expr,typ) ->
    | MapAssigndecl(expr,typ) -> 
    | Eventdecl(expr,typli) -> 
    | Constructordecl(expr,typ,typ) -> 
    | Methodecls(expr,typli,typ) -> 
  in *)

  let check_func func = 

    let check_args_type var1 t2 = 
      let check_type x1 t1 t2 =  let tag = (t1 = t2)
      and unmatch_err = "function argument " ^ string_of_expr x1 ^ " has type " 
      ^ string_of_typ t1 ^ " ,which is unmatch with declaration type " ^ string_of_typ t2 in
        match tag with 
        true -> t1
        | false -> raise (Failure unmatch_err)
      in
      match var1, t2 with
      Var(x1, t1), t2 -> check_type x1 t1 t2
      | _, _ -> raise (Failure "Not a legal variables in arguments")

    in

    let func_decl = find_func (string_of_expr func.methodname) in

    let params_types = match func_decl with 
      Methodecls(expr, typli, typ) -> typli
      | _ -> raise (Failure "Not legal method")
    in
    
    (* Check argument types length matches with declaration  *)
    let _ = let typ_len_func = List.length func.params
      in let typ_len_decl = List.length params_types in
      match typ_len_func, typ_len_decl with
      typ_len_func, typ_len_decl when (typ_len_func > typ_len_decl)
      -> raise (Failure ("Redundant arguments in method " ^ string_of_expr func.methodname))
      | typ_len_func, typ_len_decl when (typ_len_func < typ_len_decl)
      -> raise (Failure ("Missing arguments in method " ^ string_of_expr func.methodname))
      | _, _ -> typ_len_func
    in

    (* Check whether variable argument type matches with declaration *)
    let _ = (List.map2 check_args_type func.params params_types) in

    let add_var_args map var =
      let dup_err v = "duplicate variable " ^ (string_of_expr v) ^ " in method arguments"
      and make_err er = raise (Failure er)
      in match var with (* No duplicate variables or redefinitions of built-ins *)
        Var(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
      | Var(x, t) ->  StringMap.add (string_of_expr x) var map
      | _ -> raise (Failure "Only variable allows in method arguments")
    in

    let var_sym = List.fold_left add_var_args var_decls func.params in

    (* Return a variable from our symbol table *)
    let find_var s =
      try StringMap.find s var_sym
      with Not_found -> raise (Failure ("unrecognized variable " ^ s))
    in

    let rec check_expr = function
      | NumLit l -> (Int, SNumLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StrLit l -> (Void("void"), SStrLit l)
      (* check Id retrun with the correct type, keep Int for now *)
      | Id x -> (Int, SId x)
      | EnvLit(x, y) -> (Int, SEnvLit(x,y))
      | Mapexpr(e1, e2) -> (Int, SMapexpr(e1, e2))
      | Binop(e1, op, e2) -> (Int, SBinop(e1, op, e2))
      | Logexpr(e1, e2) -> (Int, SLogexpr(e1, e2))
    in
    
    { 
      smethodname = check_expr func.methodname;
      sparams = func.params;
      sguard_body = List.map check_expr func.guard_body;
      sstorage_body = List.map check_expr func.storage_body;
      seffects_body = List.map check_expr func.effects_body;
      sreturns = func.returns;
    }
  in

  let sinterface_def =
      {
        ssignaturename = check_expr signature.signaturename;
        sinterfacebody = signature.interfacebody;
      }
  in 

  let simplementation_def = 
    {
      sconsturctor = {
        sname = check_expr implementation.consturctor.name;
        sparams = implementation.consturctor.params;
        sconsturctor_body = List.map check_expr implementation.consturctor.consturctor_body;
        sreturn_type = implementation.consturctor.return_type;
      };

      smethods = List.map check_func implementation.methods;
    }
  in 

  let sprogram = (sinterface_def, simplementation_def)
  in
  sprogram

open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* build local symbol table of variabels which can be used in implementations *)
(* let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
    StringMap.empty (interfaces)
in

let type_of_id s =
  try StringMap.find s symbols 
  with Not_found -> raise ( Failure ("undeclared id" ^ s))
in *)

let check signatures =

    (* verify there is no duplicate names for signatures *)
    let signature = List.hd signatures
      in
    let check_interface  signtaure= 
        (* Debug *)
        let () = Printf.printf "asdasdasdasdasok"
        in
      let rec check_expr_list = function
        [] -> []
      | s :: sl -> check_expr s :: check_expr_list sl 
  
      and check_expr = function
      | NumLit l -> SnumLit l 
      | BooLit l ->  SbooLit l
      | StrLit l -> SstrLit l
      | Id x -> SId x (* (type_of_id x) *)
      | Var (s, t) -> Svar (s, t)
      | TypeAssign (e, t) -> StypeAssign(( check_expr e ), t)
      | MapAssign (e, t1, t2) -> SmapAssign ((check_expr e), t1, t2)
      | PointAssign (e1, e2) -> SpointAssign((check_expr e1), (check_expr e2))
      | Event (s, t) -> Sevent(s, t)
      (* | Binop (e1, op, e2) -> *)
        (* let (t1, e1') *)
      | Constructorexpr (s, t1, t2) -> Sconstructorexpr(s, t1, t2)
      | Methodexpr (s, t1, t2) -> Smethodexpr(s, t1, t2)
      (* | Logexpr (e, e2) -> *)
      in
      { 
        ssignaturename = check_expr signature.signaturename;
        sinterfacebody = check_expr_list signature.interfacebody
      }
      in
      (* Debug 
      let () = print_endline (string_of_expr signature.signaturename)
    in *)
  (check_interface signatures )(* List.map check_implementations implementations) *)



    (*  verify there is no duplicate
        let check_binds (kind : string) (binds : (typ * string) list) =
      let rec dups = function
          [] -> ()
        |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
        | _ :: t -> dups t
      in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
    in *)




    (* TODO: add implementation check !!!  *)
    (* and let rec check_stmt_list = function
        [] ->
      | s :: sl -> check_stmt s :: check_stmt_list sl 
    
    and check_stmt = function 
        Block sl -> Sblock (check_stmt sl)
      | Expr e -> Sexpr (check_expr e)
      | Return e -> 
        let (t, e') -> (check_expr e) (* return sexpr*)
        if t = method.return_type then Sreturn (t, e')
        else
          raise(
            Faiure ("you should have the correct return type in your method which match your return type")
          )
      in *)
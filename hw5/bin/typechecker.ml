open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) (err : string) = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with
  | (TInt, TInt) | (TBool, TBool) -> true
  (* Check for subtyping between ref types with subtype_ref *)
  | (TRef r1, TRef r2) | (TRef r1, TNullRef r2) | (TNullRef r1, TNullRef r2) -> subtype_ref c r1 r2
  | _, _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match t1, t2 with 
  | RString, RString -> true 
  | RStruct id1, RStruct id2 -> 
    (
      match (lookup_struct_option id1 c), (lookup_struct_option id2 c) with  
      | Some s1, Some s2 ->
        (
          let rec field_aux s1 s2 =
            match s1, s2 with
            | [], [] -> true
            (* S1 can inherit from S2 *)
            | _, [] -> true
            (* Recursively check the remaining *)
            | (f1::fs1), (f2::fs2) -> if (f1=f2) then (field_aux fs1 fs2) else false
            | [], _ -> false
          in field_aux s1 s2
        )
      | _, _ -> false 
    ) 
  | RArray ty1, RArray ty2 -> (ty1 = ty2) 
  | RFun (arg_t_l1, ret_ty1), RFun (arg_t_l2, ret_ty2) ->
    let ret_aux c (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =
      match t1, t2 with 
      | RetVoid, RetVoid -> true
      | RetVal ty1, RetVal ty2 -> subtype c ty1 ty2
      | _, _ -> false
    in
    (* length check => ret ty => one by one arg ty *)
    List.length arg_t_l1 = List.length arg_t_l2 &&
    List.for_all2 (fun t2 t1 -> subtype c t2 t1) arg_t_l2 arg_t_l1 &&
    ret_aux c ret_ty1 ret_ty2
  | _, _ -> false
  
(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with 
  | TInt -> () 
  | TBool -> () 
  | TRef rty | TNullRef rty -> typecheck_ref l tc rty

and typecheck_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with 
  | RString -> ()
  | RStruct id -> 
    (
      match lookup_struct_option id tc with 
      | None -> type_error l "Type Error: Struct not found"
      | Some s -> ()
    )
  | RArray ty -> typecheck_ty l tc ty
  | RFun (ty_l, ret_ty) ->
    (* Check args type then check the return type *)
    ty_l 
    |> List.iter (typecheck_ty l tc)
    |> (fun _ -> typecheck_ret l tc ret_ty)

and typecheck_ret (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty ->  typecheck_ty l tc ty

(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t : Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _ -> false

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups (fs : field list) =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) (id : id) (fs : field list)  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)

let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =

  let typecheck_bop (bop : Ast.binop) (e1 : Ast.exp node) (e2 : Ast.exp node) : Ast.ty =
    let aux_intop e1_ty e2_ty = 
      match e1_ty, e2_ty with 
      | TInt, TInt -> TInt
      | _, _ -> type_error e1 "Type Error: Invalid operands type for intop"
    in
    let aux_cmpop e1_ty e2_ty = 
      match e1_ty, e2_ty with 
      | TInt, TInt -> TBool
      | _, _ -> type_error e1 "Type Error: Invalid operands type for cmpop"
    in
    let aux_boolop e1_ty e2_ty = 
      match e1_ty, e2_ty with 
      | TBool, TBool -> TBool
      | _, _ -> type_error e1 "Type Error: Invalid operands type for boolop"
    in
    let e1_ty = typecheck_exp c e1 in
    let e2_ty = typecheck_exp c e2 in
    match bop with
    | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> 
      aux_intop e1_ty e2_ty
    | Lt | Lte | Gt | Gte  ->
      aux_cmpop e1_ty e2_ty
    | And | Or ->
      aux_boolop e1_ty e2_ty
    | Eq ->
      if (subtype c e1_ty e2_ty && subtype c e2_ty e1_ty) then TBool else type_error e1 "Type Error: Invalid operands type for equality"
    | Neq ->
      if (subtype c e1_ty e2_ty && subtype c e2_ty e1_ty) then TBool else type_error e1 "Type Error: Invalid operands type for inequality"
  in

  let typecheck_uop (uop : Ast.unop) (e : Ast.exp node) : Ast.ty =
    let aux_intop e_ty = 
      match e_ty with 
      | TInt -> TInt
      | _ -> type_error e "Type Error: Invalid operand type for neg/bitneg"
    in
    let aux_boolop e_ty =
      match e_ty with 
      | TBool -> TBool
      | _ -> type_error e "Type Error: Invalid operand type for lognot"
    in
    let e_ty = typecheck_exp c e in
    match uop with
    | Neg | Bitnot -> aux_intop e_ty
    | Lognot -> aux_boolop e_ty
  in

  let typecheck_carr (ty : Ast.ty) (es : Ast.exp node list) : Ast.ty =
    (* check ty => check elements => return *)
    ty
    |> (fun ty -> if(typecheck_ty e c ty = ())
      then es
      else type_error e "Type Error: Invalid array type")
    (* check type of each element => check subtype of each element *)
    |> List.map (typecheck_exp c)
    |> (fun ty_l -> if List.for_all (fun t -> subtype c t ty) ty_l 
      then ty
      else type_error e "Type Error: Invalid array type")
    |> (fun ty -> TRef (RArray ty))
  in

  let typecheck_newarr (ty : Ast.ty) (e : Ast.exp node) : Ast.ty =
    (* check ty => check e => return *)
    ty
    |> (fun ty -> if(typecheck_ty e c ty = ())
      then ty
      else type_error e "Type Error: Invalid array type")
    |> (fun ty -> match ty with
      | TInt | TBool -> e
      | TRef _ | TNullRef _ -> if is_nullable_ty ty
        then e 
        else type_error e "Type Error: Invalid array type")
    (* check e => return *)
    |> typecheck_exp c
    |> (fun _ -> TRef (RArray ty))
  in

  let typecheck_newarrinit (ty : Ast.ty) (e1 : Ast.exp node) (id : Ast.id) (e2 : Ast.exp node) : Ast.ty =
    (* check ty => check e1 => check id => check e2 => return *)
    ty
    |> (fun ty -> if(typecheck_ty e c ty = ())
      then e1
      else type_error e "Type Error: Invalid array type")
    |> (fun e1 -> match typecheck_exp c e1 with
      | TInt -> id
      | _ -> type_error e "Type Error: Invalid array type")
    |> (fun id -> match lookup_local_option id c with
      | None -> e2
      | Some _ -> type_error e "Type Error: Invalid array type")
    |> (fun e2 -> typecheck_exp (add_local c id TInt) e2)
    |> (fun ty2 -> subtype c ty2 ty)
    |> (fun b -> if b then TRef (RArray ty) else type_error e "Type Error: Invalid array type")
  in

  let typecheck_index (e1 : Ast.exp node) (e2 : Ast.exp node) : Ast.ty =
    let e1_ty = typecheck_exp c e1 in
    let e2_ty = typecheck_exp c e2 in
    match e1_ty, e2_ty with
    | TRef (RArray t), TInt -> t
    | _ -> type_error e1 "Type Error: Invalid index expression"
  in

  let typecheck_length (e : Ast.exp node) : Ast.ty =
    let e_ty = typecheck_exp c e in
    match e_ty with
    | TRef (RArray e1_ty) -> TInt
    | TNullRef (RArray e1_ty) -> TInt
    | _ -> type_error e "Type Error: Invalid length expression"
  in

  let typecheck_cstruct (id : Ast.id) (cfields : (Ast.id * Ast.exp node) list) : Ast.ty =
    let typecheck_fields (c:Tctxt.t) (id:id) (fields: (id*exp node) list) : Ast.ty = 
      List.fold_left (fun acc (fid, exp) ->
      match lookup_field_option id fid c with
      | None -> type_error exp "Type Error: Field not in context"
      | Some ty -> if subtype c (typecheck_exp c exp) ty then acc
             else type_error exp "Type Error: Not subtype"
      ) (TRef (RStruct id)) fields
    in
    match (lookup_struct_option id c) with
      | None -> type_error e "Type Error: Struct not in ctxt" 
      | Some fields ->
        if List.length cfields = List.length fields 
        then
          cfields
          |> List.map snd
          |> List.map (typecheck_exp c)
          |> (fun _ -> 
            cfields
            |> List.map (fun (id, exp) -> {fieldName = id; ftyp = typecheck_exp c exp})
            |> (fun fslist -> 
              typecheck_tdecl c id fslist e;
              typecheck_fields c id cfields
            )
          )
        else
          type_error e "Type Error: Invalid struct size"
  in

  let typecheck_proj (e : Ast.exp node) (id : Ast.id) : Ast.ty =
    match typecheck_exp c e with
    | TRef (RStruct sid) | TNullRef (RStruct sid) -> 
      let s_option = lookup_struct_option sid c in
      let f_option = lookup_field_option sid id c in
      (
        match s_option with
          | None -> type_error e "Type Error: Cannot find struct"
          | Some fl ->
            (
              match f_option with
              | None -> type_error e "Type Error: Cannot find field"
              | Some s -> s
            )
      )
    | _ -> type_error e "Type Error: Invalid struct expression"
  in

  let typecheck_call (e1 : Ast.exp node) (es : Ast.exp node list) : Ast.ty = 
    let ty_match_aux ty =
      match ty with
      | TRef (RFun (arg_t_l, RetVal ret_ty)) -> arg_t_l,es,ret_ty
      | _ -> type_error e1 "Type Error: Invalid function in call"
    in
    (* check e1 type => arg_tys,es,ret_ty *)
    (*                    ||    ||    ||  *)
    (*              check args type   ||  *)
    (*                             return *)
    e1
    |> typecheck_exp c
    |> ty_match_aux
    |> (fun (arg_t_l, es, ret_ty) -> (arg_t_l,List.map(fun e -> typecheck_exp c e) es),ret_ty)
    |> (fun ((arg_t_l, t_l), ret_ty) -> 
      if (List.length arg_t_l = List.length es) 
        then List.for_all (fun (t1,t2) -> subtype c t2 t1) (List.combine arg_t_l t_l),ret_ty
        else type_error e1 "Type Error: Invalid argument length"
      )
    |> (fun (valid,ret_ty) -> 
      if valid 
        then ret_ty
        else type_error e1 "Type Error: Invalid argument type")
  in

  match e.elt with
  | CNull rty -> 
    (* check rty => return *)
    rty |> (fun rty -> typecheck_ref e c rty) |> (fun _ -> TNullRef rty)
  | CBool b -> TBool
  | CInt i -> TInt
  | CStr s -> TRef (RString)
  | Id id -> lookup id c 
  | CArr (ty,es) -> typecheck_carr ty es
  | NewArr (ty, e) -> typecheck_newarr ty e
  | NewArrInit (ty, e1, id, e2) -> typecheck_newarrinit ty e1 id e2
  | Index (e1, e2) -> typecheck_index e1 e2
  | Length e -> typecheck_length e
  | CStruct (id, cfields) -> typecheck_cstruct id cfields
  | Proj (e, id) -> typecheck_proj e id
  | Call (e1, es) -> typecheck_call e1 es
  | Bop (bop, e1, e2) -> typecheck_bop bop e1 e2
  | Uop (uop, e) -> typecheck_uop uop e
  
(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)

     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, the return behavior of the branching 
        statement is the conjunction of the return behavior of the two 
        branches: both both branches must definitely return in order for 
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entire conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  let typecheck_while_stmt (e: Ast.exp node) (sl:Ast.stmt node list) : Tctxt.t * bool =
    let block_check = typecheck_block tc sl to_ret in
    if ((typecheck_exp tc e) <> TBool) 
      then type_error e "Type Error: Not a bool in while expression" 
      else (tc,false)
  in

  let typecheck_assn_stmt (lhs:Ast.exp node) (e:Ast.exp node) : Tctxt.t * bool = 
    let rec get_id_rec (e: Ast.exp node) : Ast.id =
      match e.elt with
      | Id id -> id
      | Index (e1,e2) -> get_id_rec e1
      | Proj (e, id) -> get_id_rec e 
      | _ -> type_error e "can't get id"
    in
    let check_id_assn (tc: Tctxt.t) (id: Ast.id) : bool = 
      let local_option = lookup_local_option id tc in
      let global_option = lookup_global_option id tc in
      local_option
      |> (fun local_option ->
        match local_option with
        | None -> false
        | Some s -> true)
      |> (fun is_local_var -> if is_local_var then true else
          match global_option with
            | None -> false
            | Some s -> 
              match s with
                | TRef (RFun (args,rt)) -> false
                | _ -> true
        )
    in
    lhs
    |> (fun lhs ->
      match lhs.elt with
        | Id id -> check_id_assn tc id
        | Index (e1,e2) -> 
          (
            match e1.elt with
            | Id _ | Index _ | Proj _ -> 
              let id = get_id_rec e1 in
              check_id_assn tc id
            | Call _ -> true
            | _ -> type_error s "Type Error: Invalid array expression for lhs"
          )
        | Proj (e,id) -> 
          let id = get_id_rec e in
          check_id_assn tc id
        | _ -> type_error s "Type Error: Invalid lhs expression"
    )
    |> (fun bool -> if bool then (typecheck_exp tc lhs),(typecheck_exp tc e) else type_error s "Type Error: Invalid lhs")
    |> (fun (lhs_ty, e_ty) -> if subtype tc e_ty lhs_ty then (tc,false) else type_error s "Type Error: Invalid assign")
  in

  let typecheck_scall_stmt (e:Ast.exp node) (el:Ast.exp node list) : Tctxt.t * bool =
    let el_t_l = List.map (typecheck_exp tc) el in
    match typecheck_exp tc e with
    | TRef (RFun (args,rt)) ->
      (
        match rt with
        | RetVal rval -> type_error e "Type Error: Return value should be void"
        | RetVoid ->
          if (List.for_all2 (subtype tc) el_t_l args) then (tc,false) else type_error e "Type Error: Argument type mismatch"
      )
    | _ -> type_error e "Type Error: Invalid function call"
  in

  let typecheck_if_stmt (e:Ast.exp node) (b1:Ast.stmt node list) (b2:Ast.stmt node list) : Tctxt.t * bool =
    if ((typecheck_exp tc e) <> TBool) 
      then type_error e "Type Error: Not a bool in if expression"
      else
        let r1 = typecheck_block tc b1 to_ret in
        let r2 = typecheck_block tc b2 to_ret in
        if (r1 && r2) then (tc,true) else (tc,false)
  in

  let typecheck_cast_stmt (cast_rty: Ast.rty) (cast_id: Ast.id) (cast_exp: Ast.exp node) (then_stmts: Ast.stmt node list) (else_stmts: Ast.stmt node list) : Tctxt.t * bool =
    match typecheck_exp tc cast_exp with
    | TNullRef exp_rty -> 
      (
        match subtype_ref tc exp_rty cast_rty with
        | true -> 
              let tc_x = add_local tc cast_id (TRef cast_rty) in
              let r1 = typecheck_block tc_x then_stmts to_ret in
              let r2 = typecheck_block tc else_stmts to_ret in
              (tc_x, r1 && r2)
        | false -> type_error s "Type Error: Invalid cast, not a subtype"
      )
    | _ -> type_error s "Type Error: Invalid cast, not a null ref"
  in

  let typecheck_for_stmt (vdecls : Ast.vdecl list) (e_opt : Ast.exp node option) (s_opt : Ast.stmt node option) (block : Ast.stmt node list) : Tctxt.t * bool =
    vdecls
    |> typecheck_vdecls tc
    |> (fun tc2 -> 
      e_opt
      |> Option.map (typecheck_exp tc2)
      |> Option.value ~default:TBool
      |> (fun ty -> if ty = TBool then tc2 else type_error s "Type Error: Not a bool in for loop")
    )
    |> (fun tc2 -> 
      s_opt
      |> Option.map (fun s_ -> typecheck_stmt tc2 s_ to_ret)
      |> (fun _ -> tc2)
    )
    |> (fun tc2 -> typecheck_block tc2 block to_ret)
    |> (fun _ -> (tc, false))
  in

  let typecheck_ret_stmt (e:Ast.exp node option) : Tctxt.t * bool =
    match to_ret, e with
        | RetVoid, None -> (tc,true)
        | RetVoid, Some s -> type_error s "Type Error: Return a value in fuction with void return type"
        | RetVal t, None -> type_error s "Type Error: Not return a value in function with non-void return type"
        | RetVal t, Some e -> 
          e
          |> (fun e -> typecheck_exp tc e)
          |> (fun e_ty -> if subtype tc e_ty t then (tc,true) else type_error s "Type Error: Return type mismatch")
  in

  match s.elt with
    | Assn (lhs,e) -> typecheck_assn_stmt lhs e
    | Decl (vdecl) -> (typecheck_vdecl tc vdecl, false)
    | SCall (e, el) -> typecheck_scall_stmt e el
    | If (e,b1, b2) -> typecheck_if_stmt e b1 b2
    | Cast (rty,id,e,then_stmt,else_stmt) -> typecheck_cast_stmt rty id e then_stmt else_stmt
    | While (e, sl) -> typecheck_while_stmt e sl
    | For (vdecls, e_opt, s_opt, block) -> typecheck_for_stmt vdecls e_opt s_opt block
    | Ret (e) -> typecheck_ret_stmt e     

and typecheck_block (tc : Tctxt.t) (s:Ast.stmt node list) (to_ret:ret_ty) : bool = 
  snd (
        List.fold_left 
        (
          fun (tc_acc, ret_acc) stmt -> 
            let tc_new, ret_new = typecheck_stmt tc_acc stmt to_ret in
            if ret_acc 
              then type_error stmt "Type Error: Return statement not at the end of the block" 
              else (tc_new, ret_new)
        ) 
        (tc, false) s
      )

and typecheck_vdecl (tc : Tctxt.t) (v: Ast.vdecl) : Tctxt.t =
  match v with
    | (id,e) -> 
      match lookup_local_option id tc with
      | Some s -> type_error e "Type Error: Identifier already in ctxt"
      | None -> (add_local tc id (typecheck_exp tc e))

and typecheck_vdecls (tc : Tctxt.t) (vl:Ast.vdecl list) : Tctxt.t =
  List.fold_left (fun acc vdecl -> typecheck_vdecl acc vdecl) tc vl

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - ensures formal parameters are distinct
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  let check_distinct args =
    let checker (seen, is_unique) elem =
      if List.mem elem seen then
        (seen, false)
      else
        (elem :: seen, is_unique)
    in
    let _, is_unique = List.fold_left checker ([], true) args in
    if is_unique then args else type_error l "Type Error: Duplicate arguments in function"
  in

  f.args
  |> check_distinct
  |> (fun args -> {locals = List.map (fun (a,b) -> (b,a)) args; globals = tc.globals; structs = tc.structs} )
  |> (fun temp_ctxt -> typecheck_block temp_ctxt f.body f.frtyp)
  |> (fun bool -> if bool then () else type_error l "Type Error: Invalid function")

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier
*)

let create_struct_ctxt (p:Ast.prog) : Tctxt.t = 
    List.fold_left (fun ctxt decl ->
      match decl with
      | Gtdecl x ->
        (match lookup_struct_option (fst x.elt) ctxt with
         | None -> Tctxt.add_struct ctxt (fst x.elt) (snd x.elt)
         | Some _ -> type_error x "Type Error: Struct already in ctxt")
      | _ -> ctxt
    ) Tctxt.empty p

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =

  let fold_builtins (tc:Tctxt.t) (builtins : (Ast.id * fty) list) : Tctxt.t =
    List.fold_left (fun ctxt (id, (args, ret)) ->
      Tctxt.add_global ctxt id (TRef (RFun (args, ret)))
    ) tc builtins
  in
  
  let func_ty (f:fdecl node) (tc:Tctxt.t): (Ast.ty) = 
    let check = (typecheck_ret f tc (f.elt.frtyp)) in 
    let arg_t_l = List.map fst f.elt.args in
        TRef(RFun (arg_t_l, f.elt.frtyp))
  in

  List.fold_left (fun ctxt decl ->
    match decl with
    | Gfdecl f ->
      (match lookup_global_option f.elt.fname ctxt with
       | None ->
         let f_ty = func_ty f ctxt in
         Tctxt.add_global ctxt f.elt.fname f_ty
       | Some _ -> type_error f "function already in ctxt")
    | _ -> ctxt
  ) tc p
  |> (fun new_ctxt -> fold_builtins new_ctxt builtins)

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun ctxt decl ->
    match decl with
    | Gvdecl g ->
      (match lookup_global_option g.elt.name ctxt with
       | None ->
         let g_ty = typecheck_exp ctxt g.elt.init in
         Tctxt.add_global ctxt g.elt.name g_ty
       | Some _ -> type_error g "Type Error: Global already in ctxt")
    | _ -> ctxt
  ) tc p


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p

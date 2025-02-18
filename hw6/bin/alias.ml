(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Stdlib.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"

  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis. 

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     (as the value being stored) may be aliased
   - Other instructions do not define pointers

 *)
let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  match i with
    | Alloca _ -> UidM.add u SymPtr.Unique d
    | Bitcast (_, op, _)
    | Gep (_, op, _) ->
      begin match op with
        | Id id -> UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
        | _ -> (UidM.add u SymPtr.MayAlias d)
      end
    (* | Store (Ptr _, op, _) -> 
      begin match op with 
        | Id id -> UidM.add id SymPtr.MayAlias d
        | _ -> d
      end
    | Load (_, op) -> UidM.add u SymPtr.MayAlias d *)
    (* ??????? what about the tips?????? *)

    | Load (Ptr (Ptr _), op) ->
      begin match op with
        | Id id -> 
          UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
        | _ -> (UidM.add u SymPtr.MayAlias d)
      end
    | Store (Ptr _, _, op) ->
      begin match op with
        | Id id -> 
          UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
        | _ -> (UidM.add u SymPtr.MayAlias d)
      end
      
    | Call (_,_,args) -> 
      let updated_map =
        List.fold_left (fun acc (t, op) ->
            match t, op with
            | Ptr _, Id id -> UidM.add id SymPtr.MayAlias acc
            | _ -> acc) d args
      in
      UidM.add u SymPtr.MayAlias updated_map
    | _ -> d
  
(* if I didn't use begin end, it will warn that situations are not all enumerated *)
  (* match i with
  | Alloca t -> UidM.add u SymPtr.Unique d
  | Gep (_, op, _) -> 
    match op with
    | Id id -> UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d) (* physical equal means memory address equals*)
    | _ -> UidM.add u SymPtr.MayAlias d
  | Bitcast (_, op, _) ->
    match op with
    | Id id -> UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
    | _ -> UidM.add u SymPtr.MayAlias d
  | Load (_, op) ->               (* store i32* %ptr, i32** %storage *)
    match op with                 (* %loaded_ptr = load i32*, i32** %storage *)
    | Id id -> UidM.add id SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
    | _ -> UidM.add u SymPtr.MayAlias d
  | Store *)



(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int = 
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* TASK: complete the "combine" operation for alias analysis.

       The alias analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful.

       It may be useful to define a helper function that knows how to take the
       meet of two SymPtr.t facts.
    *)
    let combine (ds:fact list) : fact =
      let acc_function k opt1 opt2 =
        begin match opt1, opt2 with
          | Some t1, Some t2 ->
            begin match t1, t2 with
              | SymPtr.MayAlias, _ | _, SymPtr.MayAlias -> Some SymPtr.MayAlias
              | SymPtr.Unique, _ | _, SymPtr.Unique -> Some SymPtr.Unique
              | _, _ -> Some SymPtr.UndefAlias
            end
          | Some t, _ | _, Some t -> Some t
          | _, _ -> None
        end
      in
      let main_function br1 br2 =
        UidM.merge acc_function br1 br2
      in
      List.fold_left main_function UidM.empty ds
    end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg


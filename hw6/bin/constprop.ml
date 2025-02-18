open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare (a:t) (b:t) =
      match a, b with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"

    
  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out with
     result that is computed statically (see the Int64 module)
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let op_to_symconst (op:Ll.operand) : SymConst.t =
    (
      match op with
      | Const(v) -> SymConst.Const(v)
      | Id(id) -> 
        (
          match UidM.find_opt id d with
          | Some(v) -> v
          | None -> SymConst.UndefConst
        )
      | _ -> SymConst.NonConst
    )
  in

  match i with
    | Binop(bop, _, op1, op2) -> 
      let computeValue a b =
        (
          match bop with
            | Add -> Int64.add a b
            | Sub -> Int64.sub a b
            | Mul -> Int64.mul a b
            | Shl -> Int64.shift_left a (Int64.to_int b)
            | Lshr -> Int64.shift_right_logical a (Int64.to_int b)
            | Ashr -> Int64.shift_right a (Int64.to_int b)
            | And -> Int64.logand a b
            | Or -> Int64.logor a b
            | Xor -> Int64.logxor a b
        )
        in
      (
        match (op_to_symconst op1, op_to_symconst op2) with
        | (SymConst.Const v1, SymConst.Const v2) -> 
            let result = computeValue v1 v2 in
            UidM.add u (SymConst.Const result) d
        | (SymConst.UndefConst, _) | (_, SymConst.UndefConst) -> 
            UidM.add u SymConst.UndefConst d
        | (SymConst.NonConst, _) | (_, SymConst.NonConst) -> 
            UidM.add u SymConst.NonConst d
      )

    | Icmp(cnd, _, op1, op2) -> 
      let computeValue a b =
        (
          match cnd with
            | Eq -> if Int64.equal a b then 1L else 0L
            | Ne -> if Int64.equal a b then 0L else 1L
            | Slt -> if (Int64.compare a b) == -1 then 1L else 0L
            | Sle -> if (Int64.compare a b) == 1 then 0L else 1L
            | Sgt -> if (Int64.compare a b) == 1 then 1L else 0L
            | Sge -> if (Int64.compare a b) == -1 then 0L else 1L
        )
      in
      (
        match (op_to_symconst op1, op_to_symconst op2) with
          | (SymConst.Const(v1), SymConst.Const(v2)) -> UidM.add u (SymConst.Const(computeValue v1 v2)) d
          | (SymConst.UndefConst, _) | (_, SymConst.UndefConst) -> 
            UidM.add u SymConst.UndefConst d
          | (SymConst.NonConst, _) | (_, SymConst.NonConst) ->
            UidM.add u SymConst.NonConst d
      )
    | Store(_) -> UidM.add u SymConst.UndefConst d
    (* Void call -> UndefConst *)
    | Call(Void, _, _) -> UidM.add u SymConst.UndefConst d
    | _ -> UidM.add u SymConst.NonConst d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  = 
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact = 
      let merge_symconst _ v1 v2 =
        match (v1, v2) with
          | (Some(SymConst.Const av), Some(SymConst.Const bv)) -> 
            if (av == bv) then Some(SymConst.Const av) else Some(SymConst.NonConst)

          | (Some(SymConst.Const _), Some(SymConst.NonConst)) -> 
            Some(SymConst.NonConst)

          | (Some(SymConst.NonConst), Some(SymConst.Const _)) -> 
            Some(SymConst.NonConst)

          | (Some(SymConst.Const av), _) -> Some(SymConst.Const av)
          | (_, Some(SymConst.Const bv)) -> Some(SymConst.Const bv)
          | (Some(SymConst.UndefConst), b) -> b
          | (a, Some(SymConst.UndefConst)) -> a
          | _ -> Some(SymConst.NonConst)
      in
      List.fold_left (UidM.merge merge_symconst) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right 
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in

  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    (* Retrieves the block associated with the label `l` *)
    let b = Cfg.block cfg l in 
    (* Retrieves the outgoing edges of the node *)
    let cb = Graph.uid_out cg l in

    let mod_op map (op:Ll.operand) : Ll.operand =
      match op with
        | Id id -> 
          (
            match (try (UidM.find id map) with Not_found -> SymConst.NonConst) with
              | SymConst.NonConst -> Ll.Id(id)
              | SymConst.Const(v) -> Ll.Const(v)
              | _ -> failwith "Not possible"
          )
        | _ -> op
    in

    let mod_insn uid insn : (Ll.uid * Ll.insn) =
      let map_mod_op = mod_op (cb uid) in
      (
        uid, 
        match insn with
          | Binop(bop, ty, op1, op2) -> Binop(bop, ty, map_mod_op op1, map_mod_op op2)
          | Load(ty, op) -> Load(ty, map_mod_op op)
          | Store(ty, op1, op2) -> Store(ty, map_mod_op op1, map_mod_op op2)
          | Icmp(cnd, ty, op1, op2) -> Icmp(cnd, ty, map_mod_op op1, map_mod_op op2)
          | Call(ty, op, ops) -> Call(ty, map_mod_op op, List.map (fun (t, o) -> (t, map_mod_op o)) ops)
          | Bitcast(ty1, op, ty2) -> Bitcast(ty1, map_mod_op op, ty2)
          | Gep(ty, op, ops) -> Gep(ty, map_mod_op op, List.map (map_mod_op) ops)
          | _ -> insn
      )
    in

    let mod_insns insns : (Ll.uid * Ll.insn) list =
      List.map (fun (uid, insn) -> mod_insn uid insn) insns
    in

    let mod_term utpair : (Ll.uid * Ll.terminator) = 
      let uid, term = utpair in
      let map_mod_op = mod_op (cb uid) in
      (
        uid,
        match term with
          | Ret(ty, Some(op)) -> Ret(ty, Some(map_mod_op op))
          | Cbr(op, lbl1, lbl2) -> Cbr(map_mod_op op, lbl1, lbl2)
          | _ -> term
      )
    in

    (* Modifies the block with the constant propagation information *)
    let mod_block = {insns= mod_insns b.insns; term=mod_term b.term}
    in
    Cfg.add_block l mod_block cfg
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg

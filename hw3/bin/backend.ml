(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86

module Platform = Util.Platform

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understanding this entire file and
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page. TODO: Need check all
*)


(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge



(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m


(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip) %rax.

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).
*)
let compile_operand (ctxt:ctxt) (dest:X86.operand) : Ll.operand -> ins =
  fun ll_op ->
  match ll_op with
  | Const i -> (Movq, [Imm (Lit i); dest])
  | Id uid -> (Movq, [lookup ctxt.layout uid; dest])
  | Gid gid -> (Leaq, [Ind3 (Lbl (Platform.mangle gid), Rip); dest])
  | Null -> (Movq, [Imm (Lit 0L); dest]) (* Null is the 64-bit 0 value. *)

(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: Don't forget to preserve caller-save registers (only if needed). ]

   [ NOTE: Remember, call can use labels as immediates! You shouldn't need to 
     perform any RIP-relative addressing for this one. ]

   [ NOTE: It is the caller's responsibility to clean up arguments pushed onto
     the stack, so you must free the stack space after the call returns. (But 
     see below about alignment.) ]

   [ NOTE: One important detail about the ABI besides the conventions is that, 
   at the time the [callq] instruction is invoked, %rsp *must* be 16-byte aligned.  
   However, because LLVM IR provides the Alloca instruction, which can dynamically
   allocate space on the stack, it is hard to know statically whether %rsp meets
   this alignment requirement.  Moroever: since, according to the calling 
   conventions, stack arguments numbered > 6 are pushed to the stack, we must take
   that into account when enforcing the alignment property.  

   We suggest that, for a first pass, you *ignore* %rsp alignment -- only a few of 
   the test cases rely on this behavior.  Once you have everything else working,
   you can enforce proper stack alignment at the call instructions by doing 
   these steps: 
    1. *before* pushing any arguments of the call to the stack, ensure that the
    %rsp is 16-byte aligned.  You can achieve that with the x86 instruction:
    `andq $-16, %rsp`  (which zeros out the lower 4 bits of %rsp, possibly 
    "allocating" unused padding space on the stack)

    2. if there are an *odd* number of arguments that will be pushed to the stack
    (which would break the 16-byte alignment because stack slots are 8 bytes),
    allocate an extra 8 bytes of padding on the stack. 

    3. follow the usual calling conventions - any stack arguments will still leave
    %rsp 16-byte aligned

    4. after the call returns, in addition to freeing up the stack slots used by
    arguments, if there were an odd number of slots, also free the extra padding. 

   ]
*)
let compile_call (ctxt:ctxt) (call:Ll.ty * Ll.operand * (Ll.ty * Ll.operand) list) dst : ins list =
  let (ret_ty, fn, args) = call in
  let arg_regs = [Rdi; Rsi; Rdx; Rcx; R08; R09] in
  let save_rsp = 
    [
      (* R12 is a callee saved register, use it to store Rsp temporarily *)
      (Subq, [Imm (Lit 8L); Reg Rsp]); (* Allocate space for saving R12 *)
      (Movq, [Reg R12; Ind3 (Lit 0L, Rsp)]); (* Save R12 to the stack *)
      (Movq, [Reg Rsp; Reg R12])
    ]
  in

  let align_rsp = [(Andq, [Imm (Lit (-16L)); Reg Rsp])] in

  (* Copy the first 6 args into regs and rest into stack *)
  let arg_copy_insns =
    (* aux function for taking the first n elements in args list *)
    let rec take_first_n n arg_list =
      match (n, arg_list) with
      | (0, _) -> []
      | (_, []) -> []
      | (n, arg::rest) -> arg :: take_first_n (n-1) rest
    in
    if List.length args <= 6 then
      List.mapi (fun i (_, op) -> compile_operand ctxt (Reg (List.nth arg_regs i)) op) (take_first_n (List.length args) args)
    else
      List.mapi (fun i (_, op) -> compile_operand ctxt (Reg (List.nth arg_regs i)) op) (take_first_n 6 args)
      @
      (match args with
       | _::_::_::_::_::_::rest -> 
         List.map (fun (_, op) -> match op with
             | Null -> (Pushq, [Imm (Lit 0L)])
             | Const i -> (Pushq, [Imm (Lit i)])
             | Id uid -> (Pushq, [lookup ctxt.layout uid])
             | _ -> failwith "compile_call: invalid type of arguments") rest
       | _ -> failwith "compile_call: invalid type of arguments")
      @
      let padding = if (List.length args - 6) mod 2 <> 0 then 
          [(Subq, [Imm (Lit 8L); Reg Rsp])] 
        else [] in
      padding
  in

  let store_ret = 
    match ret_ty with
    | Void -> []
    | _ -> [(Movq, [Reg Rax; lookup ctxt.layout dst])]
  in

  let call = 
    [
      compile_operand ctxt (Reg Rax) fn; 
      (Callq, [Reg Rax])
    ] 
  in

  let restore_rsp = 
    [
      (Movq, [Reg R12; Reg Rsp]); (* Restore Rsp from R12 *)
      (Popq, [Reg R12]); (* Restore R12 from the stack *)
    ]
  in

  save_rsp @ align_rsp @ arg_copy_insns @ call @ restore_rsp @ store_ret



(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelementptr, you must generate x86 code that performs
   the appropriate arithmetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes.
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty (tdecls:(tid * ty) list) (t:Ll.ty) : int =
  match t with
  | Void | Fun _ | I8 -> 0
  | I1 | I64 | Ptr _ -> 8
  | Struct tys -> 
    let rec aux acc = function
      | [] -> acc
      | ty :: tys -> aux (acc + size_ty tdecls ty) tys
    in
    aux 0 tys
  | Array (n, ty) -> n * (size_ty tdecls ty)
  | Namedt tid -> size_ty tdecls (List.assoc tid tdecls)



(* Generates code that computes a pointer value.

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

   - if t is a struct, the index must be a constant n and it
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the
       sizes of the types of the previous elements ]

   - if t is an array, the index can be any operand, and its
       value determines the offset within the array.

   - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)

let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) dst_uid : ins list =
  let open Asm in
  let (ty, _) = op in
  let (_, opreand) = op in
  let ty : Ll.ty = 
    match ty with
    | Ptr pointer -> pointer
    | _ -> failwith "gep operand not a pointer"
  in
  let ty : Ll.ty = 
    match ty with
    | Namedt tid -> lookup ctxt.tdecls tid
    | _ -> ty
  in
  let get_base_address : ins list = [compile_operand ctxt ~%Rax opreand] in
  let index_array : ins list = 
    [ compile_operand ctxt ~%Rbx (List.hd path)
    ; (Imulq, [~$(size_ty ctxt.tdecls ty); ~%Rbx])
    ; (Addq, [~%Rbx; ~%Rax])
    ]
  in
  let path : Ll.operand list = List.tl path in
  let rec index_subsequent (ty: Ll.ty) (path: Ll.operand list) : ins list =
    if path = [] then []
    else
      let ty : Ll.ty =
        match ty with
          | Namedt tid -> lookup ctxt.tdecls tid
          | _ -> ty
      in
      match ty with
        | Struct tys -> 
          let rec get_offset (tys: Ll.ty list) (index: int) : int =
            match tys with
              | [] -> failwith "struct has no types"
              | h :: t -> if index = 0 then 0 else size_ty ctxt.tdecls h + get_offset t (index - 1)
          in
          let index = match List.hd path with
                        | Const c -> Int64.to_int c
                        | _ -> failwith "index in struct not a constant"
          in
          let deeper_type : Ll.ty = List.nth tys index in
          let deeper_type : Ll.ty =
            match deeper_type with
              | Namedt tid -> lookup ctxt.tdecls tid
              | _ -> deeper_type
          in
          [Addq, [~$(get_offset tys index); ~%Rax]] @ index_subsequent deeper_type (List.tl path)
        | Array (_, subt) -> 
          [ compile_operand ctxt ~%Rbx (List.hd path)
          ; (Imulq, [~$(size_ty ctxt.tdecls subt); ~%Rbx])
          ; (Addq, [~%Rbx; ~%Rax])
          ] @ index_subsequent subt (List.tl path)
        | _ -> failwith "path is invalid"
  in

  (* Compute the full address and move it to dst_uid *)
  let result_instructions =
    if path = [] then get_base_address @ index_array
    else get_base_address @ index_array @ index_subsequent ty path
  in
  result_instructions @ [Movq, [~%Rax; lookup ctxt.layout dst_uid]]




    (* 
let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) dst_uid : ins list =
  let valid_op (op : Ll.ty * Ll.operand) : Ll.ty * Ll.operand = 
    match op with
    | Ptr _, _ -> op
    | _, _ -> failwith "compile_gep: Gep operand was not a pointer"
  in
  let (ptr_ty, ptr_op) = valid_op op in

  (
    match (ptr_ty, path) with
    | (Ptr base_ty, arr_idx::path_rest) ->
      let base_size = Int64.of_int (size_ty ctxt.tdecls base_ty) in

      let rec process_path ty path : ins list =
        match (ty, path) with
        | (Struct tys, (Const entry)::path_rest) -> begin
            let rec nthsum i (list:Ll.ty list) = match (i, list) with
              | (0L, entry::_) -> (0L, entry)
              | (i, entry::rem) -> 
                let (acc, res) = nthsum (Int64.sub i 1L) rem in 
                (Int64.add acc (Int64.of_int (size_ty ctxt.tdecls entry)), res)
              | _ -> failwith "Failed to find nth"
            in
            let (elem_offset, ty) = nthsum entry tys 
            in
            (Addq, [Imm (Lit elem_offset); Reg Rcx])::(process_path ty path_rest)
          end
        | (Array (_, arr_ty), entry::path_rest) ->
          let entry_size = Int64.of_int (size_ty ctxt.tdecls arr_ty) in
          (compile_operand ctxt (Reg Rdx) entry)::(Imulq, [Imm (Lit entry_size); Reg Rdx])::(Addq, [Reg Rdx; Reg Rcx])::(process_path arr_ty path_rest)
        (* find actual type of Namedt *)
        | (Namedt tid, path_rest) -> process_path (lookup ctxt.tdecls tid) path_rest
        | (_, []) -> []
        | (_, _) -> failwith ("Failed to match gep path")
      in

      [
        (compile_operand ctxt (Reg Rcx) ptr_op);
        (compile_operand ctxt (Reg Rdx) arr_idx);
        (* Multiply the array index by the size of the array element type *)
        (Imulq, [Imm (Lit base_size); Reg Rdx]);

        (Addq, [Reg Rdx; Reg Rcx]);
      ]
      @
      (* Process the rest of the path based on the type of the current element *)
      process_path base_ty path_rest

    | _ -> failwith "compile_gep: Gep path is empty"
  )
  @
  (* Update locals with the new pointer *)
  [Movq, [Reg Rcx; lookup ctxt.layout dst_uid]] *)


(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier. TODO: Understand where to use mangle

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let compile_insn (ctxt:ctxt) ((uid:uid), (i:Ll.insn)) : X86.ins list =
  let compile_bop ctxt bop ty op1 op2 dst_uid : ins list =
    (* Location of src and dest is different from x86 *)
    if ty <> I64 then failwith "compile_insn: invalid type of operands"
    else
      compile_operand ctxt (Reg R08) op1 ::
      compile_operand ctxt (Reg Rcx) op2 ::
      (match bop with
       | Add -> [(Addq, [Reg Rcx; Reg R08])]
       | Sub -> [(Subq, [Reg Rcx; Reg R08])]
       | Mul -> [(Imulq, [Reg Rcx; Reg R08])]
       | Shl -> [(Shlq, [Reg Rcx; Reg R08])]
       | Lshr -> [(Shrq, [Reg Rcx; Reg R08])]
       | Ashr -> [(Sarq, [Reg Rcx; Reg R08])]
       | And -> [(Andq, [Reg Rcx; Reg R08])]
       | Or -> [(Orq, [Reg Rcx; Reg R08])]
       | Xor -> [(Xorq, [Reg Rcx; Reg R08])])
      @
      [(Movq, [Reg R08; lookup ctxt.layout dst_uid])]
  in
  let compile_icmp cnd ty op1 op2 dst_uid : ins list = 
    match ty with
    (* simple type *)
    | I1 | I8 | I64 | Ptr _ -> 
      compile_operand ctxt (Reg R08) op1::
      compile_operand ctxt (Reg Rcx) op2::
      [
        (* Cmpq will only change the last bit, so clearing R09 is needed *)
        (Xorq, [Reg R09; Reg R09]);
        (Cmpq, [Reg Rcx; Reg R08]);
        (Set (compile_cnd cnd), [Reg R09]);
        (Movq, [Reg R09; lookup ctxt.layout dst_uid])
      ]
    | _ -> failwith "compile_insn: invalid type of operands"
  in
  let compile_alloca ty dst_uid : ins list =
    match ty with
    | I1 | I8 | I64 | Ptr _ -> 
      [
        (* allocate a slot *)
        (Subq, [Imm (Lit (Int64.of_int (size_ty ctxt.tdecls ty))); Reg Rsp]);
        (* return a pointer *)
        (Movq, [Reg Rsp; lookup ctxt.layout dst_uid])
      ]
    | _ -> failwith "compile_insn: invalid type of operands"
  in
  let compile_load ty op dst_uid : ins list =
    match ty with
    | Struct _ | Array _ | Fun _ | Namedt _ | Void | I1 | I8 | I64 -> failwith "compile_insn: invalid type of operands"
    | _ -> (* Contain undef type *)
      [
        compile_operand ctxt (Reg Rcx) op;
        (* Direct movq from ind2 rcx to des is invalid in x86lite *)
        (Movq, [Ind2 Rcx; Reg Rdx]);
        (Movq, [Reg Rdx; lookup ctxt.layout dst_uid])
      ]
  in
  let compile_store _ op1 op2 : ins list =
    [
      compile_operand ctxt (Reg Rcx) op1;
      compile_operand ctxt (Reg R08) op2; 
      (Movq, [Reg Rcx; Ind2 R08])
    ]
  in
  let compile_bitcast _ op _ dst_uid : ins list =
    [
      compile_operand ctxt (Reg Rcx) op;
      (Movq, [Reg Rcx; lookup ctxt.layout dst_uid])
    ]
  in
  match i with
  | Binop (bop, ty, op1, op2) -> compile_bop ctxt bop ty op1 op2 uid
  | Alloca (ty) -> compile_alloca ty uid
  | Load (ty, op) -> compile_load ty op uid
  | Icmp (cnd, ty, op1, op2) -> compile_icmp cnd ty op1 op2 uid
  | Store (ty, op1, op2) -> compile_store ty op1 op2
  | Call (ty, op, ops) -> compile_call ctxt (ty, op, ops) uid
  | Bitcast (ty, op, ty2) -> compile_bitcast ty op ty2 uid
  | Gep (ty, op, ops) -> compile_gep ctxt (ty, op) ops uid



(* compiling terminators  --------------------------------------------------- *)

(* prefix the function name [fn] to a label to ensure that the X86 labels are 
   globally unique . *)
let mk_lbl (fn:string) (l:string) = fn ^ "." ^ l

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional

   [fn] - the name of the function containing this terminator
*)
let compile_terminator (fn:string) (ctxt:ctxt) (t:Ll.terminator) : ins list =
  match t with
  | Ret (_, Some op) -> [compile_operand ctxt (Reg Rax) op; (Movq, [Reg Rbp; Reg Rsp]); (Popq, [Reg Rbp]); (Retq, [])]
  | Ret (Void, _) -> [(Movq, [Reg Rbp; Reg Rsp]); (Popq, [Reg Rbp]); (Retq, [])]
  | Br (lbl) -> [(Jmp, [Imm (Lbl (mk_lbl fn lbl))])]
  | Cbr (op, lbl1, lbl2) -> 
    [
      compile_operand ctxt (Reg R09) op;
      (Cmpq, [Imm (Lit 0L); Reg R09]);
      (J Eq, [Imm (Lbl (mk_lbl fn lbl2))]);
      (Jmp, [Imm (Lbl (mk_lbl fn lbl1))])
    ]
  | _ -> failwith "compile_terminator: invalid terminator"


(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete. 
   [fn] - the name of the function containing this block
   [ctxt] - the current context
   [blk]  - LLVM IR code for the block
*)
let compile_block (fn:string) (ctxt:ctxt) (blk:Ll.block) : ins list =
  let {insns = insns; term = (_, term)} = blk in
  let compiled_instructions : X86.ins list =
    insns |> List.map (fun insn -> compile_insn ctxt insn) |> List.concat in
  let compiled_terminator : X86.ins list = compile_terminator fn ctxt term in
  compiled_instructions @ compiled_terminator

let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)


(* Complete this helper function, which computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions. We will test this function as part of
   the hidden test cases.

   You might find it useful for compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : operand =
  (* In System V AMD64 ABI, the first 6 args are passed with regs *)
  match n with
  | 0 -> Reg Rdi
  | 1 -> Reg Rsi
  | 2 -> Reg Rdx
  | 3 -> Reg Rcx
  | 4 -> Reg R08
  | 5 -> Reg R09
  | _ -> Ind3 (Lit (Int64.of_int ((n - 5 + 1) * 8)), Rbp)


(* We suggest that you create a helper function that computes the
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id
     is also stored as a stack slot.
   - see the discussion about locals

*)
let stack_layout (args : uid list) ((block, lbled_blocks):cfg) : int * layout =
  let arg_layout : layout = List.mapi (fun i arg -> 
      if i < 6 then
        (arg, Ind3 (Lit (Int64.of_int (-((i+1) * 8))), Rbp))
      else
        (arg, arg_loc i)) 
      args
  in

  let arg_offset = 
    if List.length args < 6 then
      -((List.length args + 1) * 8)
    else
      -(6 + 1) * 8
  in

  let rec extract_insns blocks =
    match blocks with
    | [] -> []
    | {insns=insns; _} :: rest -> insns @ extract_insns rest
  in

  let all_blocks = block :: (List.map (fun (_, block) -> block) lbled_blocks)
  in

  let blocks = extract_insns all_blocks in

  let var_fold (offset, rem) (uid, insn) : int * layout = 
    let size = match insn with
      | (Binop _ | Alloca _ | Load _ | Icmp _ | Bitcast _ | Gep _ ) -> 8
      | Store _ -> 0
      | Call (Void, _, _) -> 0 (* void return type *)
      | Call _ -> 8  (* non-void return type *)
    in
    if size > 0 then
      (offset - size, (uid, Ind3 (Lit (Int64.of_int offset), Rbp))::rem)
    else
      (offset, rem)
  in

  List.fold_left var_fold (arg_offset, arg_layout) blocks

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl (tdecls:(tid * ty) list) (name:string) ({ f_param; f_cfg; _ }:fdecl) : prog =
  let (stack_offset, stack_layout) = stack_layout f_param f_cfg in

  let ctxt = { tdecls = tdecls; layout = stack_layout } in

  (* Insturctions for maintaining the stack frame *)
  let stack_init = [(Pushq, [Reg Rbp]); (Movq, [Reg Rsp; Reg Rbp])] in

  (* Insturctions for allocating the stack storage *)
  let stack_alloc = [(Addq, [Imm (Lit (Int64.of_int stack_offset)); Reg Rsp])] in

  (* Instructions for copying arguments into the stack slots *)
  let arg_copy_insns : ins list = List.concat ( List.mapi ( 
      fun i arg ->
        if i < 6 then 
          [(Movq, [arg_loc i; lookup stack_layout arg])] 
        else []
    )
      f_param
    ) 
  in

  let (entry_blk, lbl_blk) = f_cfg in

  let entry_insns = compile_block name ctxt entry_blk in

  let lbl_blk_elem = List.map (fun (lbl, blk) -> compile_lbl_block (Platform.mangle name) lbl ctxt blk) lbl_blk in

  Asm.gtext (Platform.mangle name) (stack_init @ stack_alloc @ arg_copy_insns @ entry_insns)::lbl_blk_elem



(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (_t1,g,_t2) -> compile_ginit g

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls; _} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)

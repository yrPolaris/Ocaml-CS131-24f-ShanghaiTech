(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L (* lowest valid address *)
let mem_top = 0x410000L (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17 (* including Rip *)
let ins_size = 8L (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL (* halt when m.regs(%rip) = exit_addr *)

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
   at&t syntax             ocaml syntax
   movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
   decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

   0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
   0x400001 :  InsFrag
   0x400002 :  InsFrag
   0x400003 :  InsFrag
   0x400004 :  InsFrag
   0x400005 :  InsFrag
   0x400006 :  InsFrag
   0x400007 :  InsFrag
   0x400008 :  InsB0 (Decq,  [~%Rdi])
   0x40000A :  InsFrag
   0x40000B :  InsFrag
   0x40000C :  InsFrag
   0x40000D :  InsFrag
   0x40000E :  InsFrag
   0x40000F :  InsFrag
   0x400010 :  InsFrag
*)
type sbyte =
  | InsB0 of ins (* 1st byte of an instruction *)
  | InsFrag (* 2nd - 8th bytes of an instruction *)
  | Byte of char (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags =
  { mutable fo : bool
  ; mutable fs : bool
  ; mutable fz : bool
  }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach =
  { flags : flags
  ; regs : regs
  ; mem : mem
  }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0
  | Rbx -> 1
  | Rcx -> 2
  | Rdx -> 3
  | Rsi -> 4
  | Rdi -> 5
  | Rbp -> 6
  | Rsp -> 7
  | R08 -> 8
  | R09 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
;;

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i : int64) : sbyte list =
  let open Char in
  let open Int64 in
  List.map
    (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
    [ 0; 8; 16; 24; 32; 40; 48; 56 ]
;;

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs : sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i =
    match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L
;;

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s : string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i] :: acc) (pred i)
  in
  loop [ Byte '\x00' ] @@ (String.length s - 1)
;;

(* Serialize an instruction to sbytes *)
let sbytes_of_ins ((op, args) : ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) ->
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | _ -> ()
  in
  List.iter check args;
  [ InsB0 (op, args); InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag ]
;;

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"
;;

(* It might be useful to toggle printing of intermediate states of your
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

   [if !debug_simulator then print_endline @@ string_of_ins u; ...]
*)
let debug_simulator = ref false

(* override some useful operators *)
let ( +. ) = Int64.add
let ( -. ) = Int64.sub
let ( *. ) = Int64.mul
let ( <. ) a b = Int64.compare a b < 0
let ( >. ) a b = Int64.compare a b > 0
let ( <=. ) a b = Int64.compare a b <= 0
let ( >=. ) a b = Int64.compare a b >= 0

(* Interpret a condition code with respect to the given flags. *)
(* !!! Check the Specification for Help *)
let interp_cnd { fo; fs; fz } : cnd -> bool =
  fun x ->
  match x with
  | Eq -> fz
  | Neq -> not fz
  | Lt -> fs <> fo
  | Le -> fs <> fo || fz
  | Gt -> not (fs <> fo || fz)
  | Ge -> fs = fo
;;

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr : quad) : int option =
  (* mem_bot is the lowest and mem_top is one past the highest*)
  if addr >= mem_bot && addr < mem_top
  then Some (Int64.to_int (Int64.sub addr mem_bot))
  else None
;;

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* Raise X86lite_segfault when addr is invalid. *)
let map_addr_segfault (addr : quad) : int =
  match map_addr addr with
  | Some idx -> idx
  | None -> raise X86lite_segfault
;;

(* Simulates one step of the machine:
   - fetch the instruction at %rip
   - compute the source and/or destination information from the operands
   - simulate the instruction semantics
   - update the registers and/or memory appropriately
   - set the condition flags

   We provide the basic structure of step function and helper functions.
   Implement the subroutine below to complete the step function.
   See step function to understand each subroutine and how they
   are glued together.
*)

(* Read 8 bytes from memory array *)
let readquad (m : mach) (addr : quad) : quad =
  (* Get index from address *)
  let idx = map_addr_segfault addr in
  int64_of_sbytes (Array.to_list @@ Array.sub m.mem idx 8)
;;

(* Write 8 bytes to memory array *)
let writequad (m : mach) (addr : quad) (w : quad) : unit =
  let idx = map_addr_segfault addr in
  Array.blit (Array.of_list @@ sbytes_of_int64 w) 0 m.mem idx 8
;;

let fetchins (m : mach) (addr : quad) : ins =
  let idx = map_addr_segfault addr in
  match m.mem.(idx) with
  | InsB0 (op, args) -> op, args
  | _ -> failwith "fetchins: not InsB0 at %rip"
;;

(* Compute the instruction result.
 * NOTE: See int64_overflow.ml for the definition of the return type
 *  Int64_overflow.t. *)
let interp_opcode (m : mach) (o : opcode) (args : int64 list) : Int64_overflow.t =
  let open Int64 in
  let open Int64_overflow in
  match o, args with
  | Negq, [dest] -> neg dest
  | Addq, [src; dest] -> add src dest
  | Subq, [src; dest] -> sub dest src
  | Imulq, [src; dest] -> mul src dest
  | Incq, [dest] -> succ dest
  | Decq, [dest] -> pred dest
  | Notq, [dest] -> ok @@ lognot dest
  | Andq, [src; dest] -> ok @@ logand src dest
  | Orq, [src; dest] -> ok @@ logor src dest
  | Xorq, [src; dest] -> ok @@ logxor src dest
  | Sarq, [amt; dest] -> ok @@ shift_right dest (to_int amt)
  | Shlq, [amt; dest] -> ok @@ shift_left dest (to_int amt)
  | Shrq, [amt; dest] -> ok @@ shift_right_logical dest (to_int amt)
  | Set c, [dest] -> ok @@ if interp_cnd m.flags c then 1L else 0L
  | Leaq, [ind; dest] -> ok ind
  | Movq, [src; dest] -> ok src
  | Pushq, [src] -> ok src
  | Popq, [dest] -> ok (readquad m m.regs.(rind Rsp))
  | Cmpq, [src1; src2] -> sub src2 src1
  | Jmp, [src] -> ok src
  | Callq, [src] -> ok src
  | J c, [src] -> ok (if interp_cnd m.flags c then src else m.regs.(rind Rip))
  | _ -> failwith "interp_opcode: unsupported opcode"
;;

(** Update machine state with instruction results. *)
let ins_writeback (m : mach) : ins -> int64 -> unit =
  let write_operand (m : mach) (op : operand) (v : int64) =
    match op with
    | Reg reg -> m.regs.(rind reg) <- v
    | Ind1 (Lit addr) -> writequad m addr v
    | Ind2 reg -> writequad m m.regs.(rind reg) v
    | Ind3 (Lit base, offset) -> writequad m (base +. m.regs.(rind offset)) v
    | _ -> failwith "interp_operands: invalid operand to write"
  in
  fun (op, args) value ->
    match op, args with
    | (Leaq | Movq | Andq | Orq | Xorq | Subq | Addq | Shlq | Sarq | Shrq), [ _; dest ]
    | (Notq | Decq | Incq | Negq), [ dest ]
    | Set _, [ dest ] ->
      write_operand m dest value
    | Cmpq, _ -> ()
    | J _, _ | Jmp, _ ->
      m.regs.(rind Rip) <- value
    | Imulq, [ _; Reg dest ] ->
      m.regs.(rind dest) <- value
    | _ -> failwith "ins_writeback: unsupported instruction"
;;

(* mem addr ---> mem array index *)
let interp_operands (m : mach) : ins -> int64 list =
  let read_operand (m : mach) (op : operand) =
    match op with
    | Imm (Lit i) -> i
    | Reg reg -> m.regs.(rind reg)
    | Ind1 (Lit addr) -> readquad m addr
    | Ind2 reg -> readquad m m.regs.(rind reg)
    | Ind3 (Lit base, offset) -> readquad m (base +. m.regs.(rind offset))
    | _ -> failwith "interp_operands: invalid operand to read"
  in
  function
  | oc, [] -> []
  | oc, [ operand1 ] -> [ read_operand m operand1 ]
  | oc, [ operand1; operand2 ] -> [ read_operand m operand1; read_operand m operand2 ]
  | oc, _ -> failwith "interp_operands: too many operands"
;;

(* Validate the operands of an instruction *)
(* In general, instructions that involve two operands 
   must not use two memory (Ind) operands. *)
let validate_operands : ins -> unit = function
  (* Ind DEST *)
  | Leaq, [ (Ind1 _ | Ind2 _ | Ind3 _); dest ] ->
    (match dest with
     | Reg _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  (* SRC DEST *)
  | (Movq | Andq | Orq | Xorq | Subq | Addq), [ src; dest ] ->
    (match src with
     | Imm (Lit _) | Reg _ ->
       (match dest with
        | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
        | _ -> failwith "validate_operands: operand invalid")
     | Ind1 _ | Ind2 _ | Ind3 _ ->
       (match dest with
        | Reg _ -> ()
        | _ -> failwith "validate_operands: operand invalid")
     | _ -> failwith "validate_operands: operand invalid")
  (* SRC *)
  | (Pushq | Jmp | Callq), [ src ] ->
    (match src with
     | Imm (Lit _) | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  (* DEST *)
  | (Popq | Notq | Decq | Incq | Negq), [ dest ] ->
    (match dest with
     | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  (* SRC Reg *)
  | Imulq, [ src; Reg _ ] ->
    (match src with
     | Imm (Lit _) | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  (* AMT DEST *)
  | (Sarq | Shrq | Shlq), [ amt; dest ] ->
    (match amt with
     | Imm (Lit _) | Reg Rcx ->
       (match dest with
        | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
        | _ -> failwith "validate_operands: operand invalid")
     | _ -> failwith "validate_operands: operand invalid")
  (* SRC1 SRC2 *)
  | Cmpq, [ src1; src2 ] ->
    (match src1 with
     | Imm (Lit _) | Reg _ ->
       (match src2 with
        | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
        | _ -> failwith "validate_operands: operand invalid")
     | Ind1 _ | Ind2 _ | Ind3 _ ->
       (match src2 with
        | Reg _ -> ()
        | _ -> failwith "validate_operands: operand invalid")
     | _ -> failwith "validate_operands: operand invalid")
  (* None *)
  | Retq, [] -> ()
  (* CC SRC *)
  | J _, [ src ] ->
    (match src with
     | Imm (Lit _) | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  (* CC DEST *)
  | Set _, [ dest ] ->
    (match dest with
     | Reg _ | Ind1 _ | Ind2 _ | Ind3 _ -> ()
     | _ -> failwith "validate_operands: operand invalid")
  | _ -> failwith "validate_operands not implemented"
;;

let crack : ins -> ins list = function
  | Pushq, [src] -> 
    [ Subq, [Imm (Lit 8L); Reg Rsp]; Movq, [src; Ind2 Rsp] ]
  | Popq, [dest] -> 
    [ Movq, [Ind2 Rsp; dest]; Addq, [Imm (Lit 8L); Reg Rsp] ]
  | Callq, [src] -> 
    [ Subq, [Imm (Lit 8L); Reg Rsp]; Movq, [Reg Rip; Ind2 Rsp]; Jmp, [src] ]
  | Retq, [] -> 
    [ Movq, [Ind2 Rsp; Reg Rip]; Addq, [Imm (Lit 8L); Reg Rsp] ]
  | ins -> [ins]
;;

let set_flags (m : mach) (op : opcode) (ws : quad list) (w : Int64_overflow.t) : unit =
  let open Int64_overflow in
  let res = w.value in
  let overflow = w.overflow in
  let set_fs_fz (value : int64) : unit =
    if value <. 0L
    then m.flags.fs <- true
    else m.flags.fs <- false;
    if value = 0L
    then m.flags.fz <- true
    else m.flags.fz <- false
  in
  match op with
  | Negq | Addq | Subq | Imulq | Incq | Decq | Andq | Orq | Xorq | Cmpq ->
    set_fs_fz res;
    m.flags.fo <- overflow
  | Sarq -> (match ws with
      | [0L; _] -> ()
      | [1L; _] -> m.flags.fo <- false; set_fs_fz res
      | _ -> set_fs_fz res)
  | Shlq -> (match ws with
      | [amt; dest] ->
        if amt = 0L then ()
        else (
          set_fs_fz res;
          if amt = 1L then
            let top_two_bits = Int64.shift_right_logical dest 62 in
            m.flags.fo <- (Int64.to_int top_two_bits land 1) <> (Int64.to_int top_two_bits land 2)
        )
      | _ -> failwith "set_flags: invalid operands for Shlq")
  | Shrq -> (match ws with
      | [amt; dest] ->
        if amt = 0L then ()
        else (
          set_fs_fz res;
          if amt = 1L then
            m.flags.fo <- Int64.(logand dest 0x8000000000000000L <> 0L)
        )
      | _ -> failwith "set_flags: invalid operands for Shrq")
  | _ -> ()
;;

let step (m : mach) : unit =
  (* execute an instruction *)
  let ((op, args) as ins) = fetchins m m.regs.(rind Rip) in
  validate_operands ins;
  (* Some instructions involve running two or more basic instructions. 
   * For other instructions, just return a list of one instruction.
   * See the X86lite specification for details. *)
  let uops : ins list = crack (op, args) in
  m.regs.(rind Rip) <- m.regs.(rind Rip) +. ins_size;
  List.iter
    (fun ((uop, _) as u) ->
       if !debug_simulator then print_endline @@ string_of_ins u;
       let ws = interp_operands m u in
       let res = interp_opcode m uop ws in
       ins_writeback m u @@ res.Int64_overflow.value;
       set_flags m op ws res)
    uops
;;

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the
   machine halts. *)
let run (m : mach) : int64 =
  while m.regs.(rind Rip) <> exit_addr do
    step m
  done;
  m.regs.(rind Rax)
;;

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec =
  { entry : quad (* address of the entry point *)
  ; text_pos : quad (* starting address of the code *)
  ; data_pos : quad (* starting address of the data *)
  ; text_seg : sbyte list (* contents of the text segment *)
  ; data_seg : sbyte list (* contents of the data segment *)
  }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
     Note: the size of an Asciz string section is (1 + the string length)
     due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to
     replace Lbl values with the corresponding Imm values.
     HINT: consider building a mapping from symboli Lbl to memory address

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

   HINT: List.fold_left and List.fold_right are your friends.
*)
let is_size (is : ins list) : quad = 
  (* multiple  the length of list and the length of ins*)
  (Int64.of_int (List.length is)) *. ins_size

let ds_size (ds : data list) : quad = 
  (* List.fold_left : ('a -> 'b -> 'a)[one function] -> 'a[one sum value] -> 'b list[one list to be sum] -> 'a[return value]  *)
  List.fold_left (fun (accumulator : quad) (d : data) ->
    match d with
    | Asciz str -> accumulator +. (Int64.of_int (String.length str + 1))
    | Quad _ -> accumulator +. 8L
  ) 0L ds

let assemble (p : prog) : exec =
  (* text_pos and data_pos *)
  let text_pos : quad = 0x400000L in
  let text_length : quad =
    List.fold_left (fun accumulator elem ->
      match elem.asm with
      | Text ins_list -> accumulator +. (Int64.of_int (List.length ins_list) *. 8L)
      | Data _ -> accumulator
    ) 0L p
  in
  let data_pos = text_pos +. text_length in

  (* construct a symbol table *)
  let current_addr : quad ref = ref text_pos in
  let symbol_table : (lbl * quad) list =
    List.fold_left (fun sym_tbl elem ->
      match elem.asm with
      | Text is ->  let pc_addr = !current_addr in
                    let sym_tbl =
                      if List.mem_assoc elem.lbl sym_tbl then
                        raise (Redefined_sym elem.lbl)
                      else
                        (elem.lbl, pc_addr) :: sym_tbl
                    in
                    current_addr := !current_addr +. (is_size is);
                    sym_tbl
      | Data ds ->  let pc_addr = !current_addr in
                    let sym_tbl =
                      if List.mem_assoc elem.lbl sym_tbl then
                        raise (Redefined_sym elem.lbl)
                      else
                        (elem.lbl, pc_addr) :: sym_tbl
                    in
                    current_addr := !current_addr +. (ds_size ds);
                    sym_tbl
     ) [] p |> List.rev
  in

  let (text_seg, data_seg) =
   List.fold_left (fun (t_seg, d_seg) elem ->       (* accumulate t_seg, d_seg through list with element elem *)
    match elem.asm with
    | Text instruction ->
      let trans_ins = List.map (fun (opcode, operand) ->             (* outside map maps ins to trans_ins *)
        (opcode, List.map (function                              (* inside map maps label of args to absolute address *)
        | Imm (Lbl lbl) -> Imm (Lit (try List.assoc lbl symbol_table with Not_found -> raise (Undefined_sym lbl)))
        | Ind1 (Lbl lbl) -> Ind1 (Lit (try List.assoc lbl symbol_table with Not_found -> raise (Undefined_sym lbl)))
        | Ind3 (Lbl lbl, reg) -> Ind3 (Lit (try List.assoc lbl symbol_table with Not_found -> raise (Undefined_sym lbl)), reg)
        | opcode -> opcode) operand)
      ) instruction in
      let t_seg = t_seg @ (List.concat (List.map sbytes_of_ins trans_ins)) in (t_seg, d_seg)
    | Data data ->
      let trans_data = List.map (function
      | Quad (Lbl lbl) -> Quad (Lit (try List.assoc lbl symbol_table with Not_found -> raise (Undefined_sym lbl)))
      | data -> data) data in
      let d_seg = d_seg @ (List.concat (List.map sbytes_of_data trans_data)) in (t_seg, d_seg)
    ) ([], []) p 
  in
  
  let entry : quad =
    try List.assoc "main" symbol_table with
    | Not_found -> raise (Undefined_sym "main")
  in
  {entry; text_pos; data_pos; text_seg; data_seg;}

(* Convert an object file into an executable machine state.
   - allocate the mem array
   - set up the memory state by writing the symbolic bytes to the
     appropriate locations
   - create the inital register state
   - initialize rip to the entry point address
   - initializes rsp to the last word in memory
   - the other registers are initialized to 0
   - the condition code flags start as 'false'

   Hint: The Array.make, Array.blit, and Array.of_list library functions
   may be of use.
*)
let load { entry; text_pos; data_pos; text_seg; data_seg } : mach =
  let mem = Array.make mem_size (Byte '\x00') in

  let text_array = Array.of_list text_seg in
  let data_array = Array.of_list data_seg in
  let text_idx = Int64.to_int (text_pos -. mem_bot) in
  let data_idx = Int64.to_int (data_pos -. mem_bot) in
  let text_length = List.length text_seg in
  let data_length = List.length data_seg in
  Array.blit text_array 0 mem text_idx text_length;
  Array.blit data_array 0 mem data_idx data_length;

  let regs :int64 array = Array.make nregs 0L in
  regs.(rind Rip) <- entry;
  regs.(rind Rsp) <- (mem_top -. 8L);
  let exit_addr_seg = sbytes_of_int64 exit_addr in
  let exit_addr_array = Array.of_list exit_addr_seg in
  let exit_addr_idx = Int64.to_int (mem_top -. mem_bot -. 8L) in
  Array.blit exit_addr_array 0 mem exit_addr_idx 8;
  let flags = { fo = false; fs = false; fz = false} in
  { flags; regs; mem }
;;
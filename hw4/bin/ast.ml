(* The type declarations in this file correspond to productions in the Oat
 * grammar provided in the Oat v.1 Language Specification. *)

module Range = Util.Range

(* The `node` type represents a node in an abstract syntax tree. 
 * The `elt` is the AST element, the `loc` contains the line number
 * info of the corresponding concrete syntax element. The parser uses
 * the `loc` to give line numbers with error messages, and you can do 
 * so too in the compiler if you like!
 *)
type 'a node = { elt : 'a; loc : Range.t }

(** val no_loc : 'a1 -> 'a1 node **)

(* This function might be useful if some of your helper functions 
 * have to return a `node` type.
 *)
let no_loc x =
  { elt = x; loc = Range.norange }

type id = string

(* Oat types *)
type ty =
| TBool
| TInt
| TRef of rty
and rty =
| RString
| RArray of ty
| RFun of ty list * ret_ty
and ret_ty =
| RetVoid
| RetVal of ty

type unop =
| Neg
| Lognot
| Bitnot

type binop =
| Add
| Sub
| Mul
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte
| And
| Or
| IAnd
| IOr
| Shl
| Shr
| Sar

(* Oat expressions *)
type exp =
| CNull of rty
| CBool of bool
| CInt of int64
| CStr of string
| CArr of ty * exp node list
| NewArr of ty * exp node
| Id of id
| Index of exp node * exp node
| Call of exp node * exp node list
| Bop of binop * exp node * exp node
| Uop of unop * exp node

type cfield = id * exp node

type vdecl = id * exp node

type stmt =
| Assn of exp node * exp node
| Decl of vdecl
| Ret of exp node option
| SCall of exp node * exp node list
| If of exp node * stmt node list * stmt node list
| For of vdecl list * exp node option * stmt node option * stmt node list
| While of exp node * stmt node list

type block = stmt node list

type gdecl = { name : id; init : exp node }

type fdecl = { frtyp : ret_ty; fname : id; args : (ty * id) list; body : block }

type field = { fieldName : id; ftyp : ty }

type decl =
| Gvdecl of gdecl node
| Gfdecl of fdecl node

type prog = decl list

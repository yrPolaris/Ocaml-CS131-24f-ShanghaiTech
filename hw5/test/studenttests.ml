open Testlib
open Util.Assert
open X86
open Ll
module Driver = Oat.Driver
module Backend = Oat.Backend
module Typechecker = Oat.Typechecker
module Frontend = Oat.Frontend
module Tctxt = Oat.Tctxt
open Backend
open Driver

(* Use this file to create additional test cases here to help you   *)
(* debug your comiplper                                             *)

let student_unit_tests_pos = [
  "typecheck_exp_uop_neg",
  (fun () ->
    let uop_op : Oat.Ast.exp Oat.Ast.node = { elt = CInt 1L; loc = Range.norange } in
    let node : Oat.Ast.exp Oat.Ast.node = { elt = Uop (Neg, uop_op); loc = Range.norange } in
    if Typechecker.typecheck_exp Tctxt.empty node = TInt then ()
      else failwith "should be TInt");
]

let student_unit_tests_neg = [
  "typecheck_ret_fail",
  (fun () ->
    let ret_ty : Oat.Ast.ret_ty = RetVal TBool in
    let exp_node : Oat.Ast.exp Oat.Ast.node = { elt = CInt 1L; loc = Range.norange } in
    let node : Oat.Ast.stmt Oat.Ast.node = { elt = Ret (Some exp_node); loc = Range.norange } in
    try 
      ignore (Typechecker.typecheck_stmt Tctxt.empty node ret_ty); 
      failwith "Should have a type error" 
    with Typechecker.TypeError s -> () );
]

let student_local_tests : suite = [
  Test ("Student Unit Test Positive", student_unit_tests_pos);
  Test ("Student Unit Test Negative", student_unit_tests_neg);
  Test ("Student File Test", executed_oat_file [("hw5programs/studenttest.oat", "", "Graph contains negative weight cycle.0")]);
]

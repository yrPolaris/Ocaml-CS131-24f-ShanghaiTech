open Util.Assert
open Gradedtests
(* You should add additional test cases here to help you   *)
(* debug your program.                                          *)

(* NOTE: Your "submitted public test case" belongs over in the 
   shared git submodule.   
*)

let studenttests =
   [("hw4programs/studenttests.oat", "", "089750")]

let provided_tests : suite = [
   GradedTest("studenttests", 100, executed_oat_file studenttests)
] 

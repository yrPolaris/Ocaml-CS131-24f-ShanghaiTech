open Util.Assert
open Gradedtests
    

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let studenttests =
    [ "llprograms/studenttests.ll", 30L]

let provided_tests : suite = [
    GradedTest("studenttests", 100, executed studenttests)
] 

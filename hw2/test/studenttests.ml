open Util.Assert
open X86
open Sim.Simulator
open Gradedtests
open Asm 

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let gcd (a:int) (b:int) : prog = 
  let open Asm in
  [
    text "gcd"
    [ Cmpq, [~%Rsi; ~%Rdi]
    ; J Eq, [~$$"exit"]
    ; J Le, [~$$"swap"]
    ; Subq, [~%Rsi; ~%Rdi]
    ; Jmp, [~$$"gcd"]
    ];
    text "swap"
    [ Movq, [~%Rsi; ~%R09]
    ; Movq, [~%Rdi; ~%Rsi]
    ; Movq, [~%R09; ~%Rdi]
    ; Jmp, [~$$"gcd"]
    ];
    text "exit"
    [ Movq, [~%Rdi; ~%Rax]
    ; Retq, []
    ];
    gtext "main"
    [ Movq, [~$a; ~%Rdi]
    ; Movq, [~$b; ~%Rsi]
    ; Callq, [~$$"gcd"]
    ; Retq, []
    ]
  ]



let gcd_test (a:int) (b:int) =
  program_test (gcd a b)

let provided_tests : suite = [
  Test("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase",
  [("gcd1", (gcd_test 720 1440) 720L);
  ("gcd2", (gcd_test 103841590 22067505) 5L)]);
]
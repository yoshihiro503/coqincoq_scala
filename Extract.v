Require Import Termes.
Require Import Conv.
Require Import Types.
Require Import Conv_Dec.
Require Import Infer.
Require Import Names.
Require Import Expr.
Require Import Machine.

Extraction Language Scala.

(* integers *)

Extract Inlined Constant ml_int => "Int".
Extract Constant ml_eq_int => "((x:Int) => (y:Int) => x == y)". 
Extract Constant ml_zero => "0".
Extract Constant ml_int_case => 
  "{case 0 => None; case n => Some(n-1)}".
Extract Inlined Constant ml_succ => "((x:Int) => x + 1)".

(* strings *)

Extract Inlined Constant ml_string => "String".
Extract Constant ml_eq_string =>
  "((s1:String) => (s2:String) => if (s1 == s2) Left() else Right())".
Extract Constant ml_x_int => "((n: Int) => ""x"" + n.toInt)".

Extraction
 NoInline list_index is_free_var check_typ red_to_sort red_to_prod exec_axiom
         glob_ctx glob_names empty_state name_dec find_free_var synthesis
         interp_command transl_message transl_error interp_ast.

Extraction "Core" is_free_var empty_state interp_ast.

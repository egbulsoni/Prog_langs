(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["A","b","C"] = ["A","C"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","ac","bc","C"] = "ac"
val test2_2 = longest_string1 ["A","ac","cbc","C"] = "cbc"
val test2_3 = longest_string1 ["A","ac","bc", "ad", "C"] = "ac"
val test2_4 = longest_string1 [] = ""



val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","ac","bc","C"] = "bc"
val test3_2 = longest_string2 ["A","ac","cbc","C"] = "cbc"
val test3_3 = longest_string2 ["A","ac","bc", "ad", "C"] = "ad"
val test3_4 = longest_string2 [] = ""
(*
val test 4c = longest_string_helper((fn (fst, snd) => fst > snd), ["A","ac","bc","C"]) = "ac"
val test 4d = longest_string_helper((fn (fst, snd) => fst >= snd), ["A","ac","bc","C"]) = "bc"
*)

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test4 = longest_string3 ["A","bc","C"] = "bc"
val test4_1 = longest_string3 ["A","ac","bc","C"] = "ac"
val test4_2 = longest_string3 ["A","ac","cbc","C"] = "cbc"
val test4_3 = longest_string3 ["A","ac","bc", "ad", "C"] = "ac"


val test41 = longest_string4 ["A","bc","C"] = "bc"
val test41_1 = longest_string4 ["A","ac","bc","C"] = "bc"
val test41_2 = longest_string4 ["A","ac","cbc","C"] = "cbc"
val test41_3 = longest_string4 ["A","ac","bc", "ad", "C"] = "ad"

val test4_4 = longest_string3 [] = ""
val test41_4 = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
(* first_answer (fn x => if x = 0 then SOME x else NONE) [1,2,3,4,5]; *)

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]



val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (TupleP [Wildcard, UnitP, TupleP [Wildcard, Wildcard] ]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("y", Variable("x")) = 0
val test9c_2 = count_some_var ("y", (TupleP [Variable("x"), Variable("Y")]) ) = 0
val test9c_3 = count_some_var ("y", (TupleP [Variable("x"), Variable("y"), Variable("y")]) ) = 2
(*
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

(* TEST *)

val test1_1 = only_capitals ["One", "Two", "three", "fOUR","FIVE"] = ["One", "Two", "FIVE"]

(* /TEST *) 

val test2 = longest_string1 ["A","bc","C"] = "bc"

(* TEST *)

val test2_1 = longest_string1 ["One", "Two", "three", "doggy", "fOUR","FIVE"] = "three"
val test2_2 = longest_string1 [] = ""

(* /TEST *) 

val test3 = longest_string2 ["A","bc","C"] = "bc"

(* TEST *)

val test3_1 = longest_string2 ["One", "Two", "three", "doggy", "fOUR","FIVE"] = "doggy"
val test3_2 = longest_string2 [] = ""

(* /TEST *) 

val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

(* TEST *)

val test4_1 = longest_string3 ["One", "Two", "three", "doggy", "fOUR","FIVE"] = "three"
val test4_2 = longest_string3 [] = ""
val test4_3 = longest_string4 ["One", "Two", "three", "doggy", "fOUR","FIVE"] = "doggy"
val test4_4 = longest_string4 [] = ""

(* /TEST *) 

val test5 = longest_capitalized ["A","bc","C"] = "A";

(* TEST *)

val test5_1 = longest_capitalized ["One", "Two", "three", "doggy", "fOUR","FIVE"] = "FIVE"
val test5_2 = longest_capitalized [] = ""

(* /TEST *) 

val test6 = rev_string "abc" = "cba";

(* TEST *)

val test6_1 = rev_string "!desreveR" = "Reversed!"

val test6_2 = rev_string "" = ""

(* /TEST *) 

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8_2 = all_answers (fn x => if x > 1 then SOME [~x] else NONE)[2,3,4,5,6,7] = SOME [~2,~3,~4,~5,~6,~7]

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1;


(* TEST *)

val p1 = (ConstructorP ("wild",Wildcard))

val test9a_1 = count_wildcards p1 = 1

val test9b_1 = count_wild_and_variable_lengths p1 = 1

val test9c_1 = count_some_var ("wild",p1) = 0;

val p2 = (TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard),TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]])

val test9a_2 = count_wildcards p2 = 4

val test9b_2 = count_wild_and_variable_lengths p2 = 6

val test9c_2 = count_some_var ("x",p2) = 2;

val test9c_3 = count_some_var ("y",TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard),TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]]) = 0

val test9c_4 = count_some_var ("x",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 2

val test9c_5 = count_some_var ("y",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 0

val test9c_6 = count_some_var ("wild",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 0

(* /TEST *)

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []


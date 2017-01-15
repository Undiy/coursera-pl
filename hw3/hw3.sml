(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard          => f1 ()
          | Variable x        => f2 x
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | ConstructorP(_,p) => r p
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals strings =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) strings

(* 2 *)
fun longest_string1 strings =
    (* using map first to avoid recomputation of string length *)
    let val slengths = List.map (fn s => (String.size s, s)) strings
    (* I hope, you won't consider this bad usage of "#" operator, 
       just returning the string from (length, string) tuple *) 
    in #2 (foldl (fn ((l1,s1),(l2,s2)) => if l1 > l2 then (l1,s1) else (l2,s2)) (0,"") slengths) 
    end

(* 3 *)
fun longest_string2 strings =
    let val slengths = List.map (fn s => (String.size s, s)) strings
    in #2 (foldl (fn ((l1,s1),(l2,s2)) => if l1 >= l2 then (l1,s1) else (l2,s2)) (0,"") slengths) 
    end

(* 4 *)
fun longest_string_helper f strings = 
    let val slengths = List.map (fn s => (String.size s, s)) strings
    in #2 (foldl (fn ((l1,s1),(l2,s2)) => if f(l1,l2) then (l1,s1) else (l2,s2)) (0,"") slengths)
    end

val longest_string3 = longest_string_helper (op >)
val longest_string4 = longest_string_helper (op >=)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
     | x::xs => case f(x) of 
                    SOME v => v
                 | NONE => first_answer f xs

(* 8 *)
fun all_answers f xs =
    case xs of 
        [] => SOME []
     | x::xs => case (f x, all_answers f xs) of 
                  (SOME lst, SOME lsts) => SOME (lst @ lsts) 
                 | _ => NONE 

(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* 9c *)
fun  count_some_var (s,p) =
     g (fn _ => 0) (fn v => if v = s then 1 else 0) p

(* 10 *)
fun check_pat p = 
    let
        fun get_names p = 
                case p of
                    Variable s => [s]
                  | ConstructorP(_,p) => get_names p
                  | TupleP ps => List.foldl (fn (p,i) => get_names(p) @ i) [] ps
                  | _  => []
 
        fun has_repeats xs =
            case xs of
                [] => false
              | x::xs  => (List.exists (fn x' => x = x') xs) orelse (has_repeats xs) 
    in
        not(has_repeats(get_names p))
    end
    
(* 11 *)
fun match (v,p) =
    case (v,p) of
         (_,Wildcard) => SOME []
       | (_,Variable s) => SOME [(s,v)]
       | (Unit,UnitP) => SOME []
       | (Const x',ConstP x) => if x=x' then SOME [] else NONE
       | (Tuple vs,TupleP ps) => if (List.length ps) = (List.length vs)
                                 then all_answers match (ListPair.zip(vs,ps))
                                 else NONE                                
       | (Constructor (s',v),ConstructorP (s,p)) => if s = s' then match(v,p) else NONE
       | _  => NONE 

(* 12 *)
fun first_match v ps =
    let val pairs = List.map (fn p => (v,p)) ps
    in SOME (first_answer match pairs) handle NoAnswer => NONE
    end

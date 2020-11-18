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


fun only_capitals(words: string list) =
	List.filter((fn word => Char.isUpper(String.sub(word,0)))) words;

fun longest_string1(words: string list)=
	List.foldl (fn (cur, long) => if String.size cur > String.size long then cur else long) "" words;

fun longest_string2(words: string list)=
	List.foldl (fn (cur, long) => if String.size cur >= String.size long then cur else long) "" words;

fun longest_string_helper f words =
	List.foldl (fn (cur, long) => if f(String.size cur, String.size long) then cur else long) "" words;

fun longest_string3(words: string list)=
	let val gt = (fn (fst, snd) => fst > snd)
	in
		(longest_string_helper gt) words
	end

fun longest_string4(words: string list)=
	let val gteq = (fn (fst, snd) => fst >= snd)
	in
		(longest_string_helper gteq) words
	end

fun longest_capitalized(words: string list) =
	let val comp = longest_string1 o only_capitals
	in comp words end

fun rev_string(word: string) =
	(String.implode o List.rev o String.explode) word


fun first_answer f optlist =
	let val answers = (List.mapPartial (f) optlist)
	in if List.null(answers) then raise NoAnswer else hd answers end


fun all_answers f a =
	let val optlist = List.map f a
	fun helper(ls, acc) =
		case ls of
			[] => SOME acc
			| x::xs => case x of
									NONE => NONE
									| SOME e => helper(xs, acc @ e)
	in
		helper(optlist, [])
	end

(*
THINGS TO HELP REASONING - IGNORE

 (’a -> ’b list option) -> ’a list -> ’b list option
'a = int
'b = int
(int -> int list option) -> int list -> int list option

fun all_answers f a =
	let val optlist = List.map f a
	fun helper(fullList, acc) =
		case fullList of
			[] => SOME acc
			| x::xs => case x of 
						NONE => NONE
						| SOME e => helper(xs, (acc @ e))
						
	in
		helper(a, [])
	end

val all_answers = fn
  : ('a list option -> 'b) -> 'a list option list -> 'a list option

type: int list option list
[SOME [2],SOME [3],SOME [4],SOME [5],SOME [6],SOME [7]]

List.concat
List.map valOf optlist

list option list
>>> return -> SOME [1,2,3] : int list option

first_answer of type (’a -> ’b option) -> ’a list -> ’b

- List.foldl; 
val it = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

helper => (int * int -> bool) -> string list -> string

List.foldl op+ 0 [5,2,4]; 

- String.size;
val it = fn : string -> int

- List.foldl (fn (bit, sumSoFar) => 2*sumSoFar + bit) 0 [1, 0, 1, 0];


Char.isUpper;
val it = fn : char -> bool

- String.sub;
val it = fn : string * int -> char

- List.filter;
val it = fn : ('a -> bool) -> 'a list -> 'a list

*)
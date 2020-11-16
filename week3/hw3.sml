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
	List.foldl (fn (cur, long) => if String.size long > String.size cur then long else cur) "" words;

(* THINGS TO HELP REASONING - IGNORE
- List.foldl; 
val it = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

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

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

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

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun check_pat(pat: pattern) =
  let fun uniqueStrings(words: string list) =
	case words of
    [] => true
    | x::xs => if (List.exists (fn ys => x = ys) xs)
                then false
                else uniqueStrings(xs)

  fun getNames (p: pattern, acc: string list) =
  case p of
    Variable v => acc @ [v]
    | TupleP v => acc @ foldl getNames [] v
    | ConstructorP (_, v) => getNames(v, acc)
    | _ => acc
  in
    (uniqueStrings o getNames) (pat, [])
  end

fun match(v: valu, p: pattern) =

(*
val match = valu * pattern -> (string * valu) list option
val all_answers = fn : ('a -> 'b list option) -> 'a list -> 'b list option
listpair.zip = fn : 'a list * 'b list -> ('a * 'b) list

*)

	

(* 

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

	*)


(* fun first_match v ps: pattern list =
	first_answer (fn x => if (x = v) then SOME x else NONE) ps handle NoAnswer => NONE *)

(*

THINGS TO HELP REASONING - IGNORE


- g;
val it = fn : (unit -> int) -> (string -> int) -> pattern -> int

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

val g = fn : (unit -> int) -> (string -> int) -> pattern -> int

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

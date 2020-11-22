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

fun g f1 f2 pat =
    let 
	val r = g f1 f2
    in
	case pat of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (pat,i) => (r pat) + i) 0 ps
	  | ConstructorP(_,pat) => r pat
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
	List.foldr (fn (cur, long) => if String.size cur > String.size long then cur else long) "" words;

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

fun count_wildcards(pat: pattern) =
	g (fn x => 1) (String.size) pat


fun count_wild_and_variable_lengths(pat: pattern) =
	g (fn x => 1) (String.size) pat

fun count_some_var(s: string, pat: pattern) =
	g (fn x => 1) (fn (y) => if y = s then 1 else 0) pat

fun check_pat(pat: pattern) =
  let fun uniqueStrings(words: string list) =
	case words of
    [] => true
    | x::xs => if (List.exists (fn ys => x = ys) xs)
                then false
                else uniqueStrings(xs)

fun getNames (pat: pattern, acc: string list) =
  case pat of
    Variable v => acc @ [v]
    | TupleP v => acc @ foldl getNames [] v
    | ConstructorP (_, v) => getNames(v, acc)
    | _ => acc
  in
    (uniqueStrings o getNames) (pat, [])
  end

	(* match = (valu * pattern) -> (string * value) list option *)
fun match (v: valu, p: pattern) =
	case (p, v) of
		   (Wildcard, _) => SOME []
		 | (Variable s, v) => SOME [(s,v)]
		 | (UnitP, Unit) => SOME []
		 | (ConstP x, Const y) => if x = y then SOME [] else NONE
		 | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
		 														then all_answers match (ListPair.zip(vs, ps))
																else NONE
		 | (ConstructorP (s1, p), Constructor (s2, v)) => if (s1 = s2) 
		 																									then match(v, p) else NONE
		 | (_, _) => NONE

fun first_match v p = 
  SOME (first_answer (fn x => match(v, x)) p)
  handle NoAnswer => NONE
(* datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu *)
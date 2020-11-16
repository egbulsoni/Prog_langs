(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*
  string * string list -> (string list) option
  return NONE if string is not in list, else return SOME lst where lst is identical to argument list except the string
  is not in it.
  Assume: string is in the list at most once.
*)
fun all_except_option(str, lst) =
    case lst of
	[] => NONE
      | x::xs => if same_string(x, str)
		 then SOME xs
		 else case all_except_option(str, xs) of
			  NONE => NONE
			| SOME l => SOME (x::l)

(*
  (string list) list * string -> string list
  produces list of all strings that are in some list in substitutions that also has s, but s itself should not be in result
*)
fun get_substitutions1(subs, str) =
    case subs of
	[] => []
      | sub::subs' => case all_except_option(str, sub) of
			  NONE => get_substitutions1(subs', str)
			| SOME l => l @ get_substitutions1(subs', str)

(*
  same as get_substitutions1, except this uses tail recursion
*)
fun get_substitutions2(subs, str) =
    let fun f(subs, acc) =
	    case subs of
		[] => acc
	      | sub::subs' => case all_except_option(str, sub) of
				  NONE => f(subs', acc)
				| SOME lst => f(subs', acc @ lst)
    in f(subs, [])
    end

(*
  (string list) list * {first:string, middle:string, last:string} -> {first:string, middle:string, last:string} list
  produces list of all full names by substituting the first name using substitutions from the list produced by get_substitutions
  answer should begin with the original name
*)
fun similar_names(subs, fullname) =
    let val {first=f, middle=m, last=l} = fullname
	fun replace(sublst) =
	    case sublst of
		[] => []
	      | sub::subs => {first=sub, middle=m, last=l}::replace(subs)
    in
	fullname::replace(get_substitutions2(subs, f))
    end
								  		
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(c) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds,_) => Red
      | (Hearts,_)   => Red

fun card_value(c) =
    case c of
	(_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | c'::cs' => if c = c'
		   then cs'
		   else c'::remove_card(cs', c, e)

fun all_same_color(cs) =
    case cs of
	c1::c2::cs' => card_color(c1) = card_color(c2)
		       andalso all_same_color(c2::cs')
      | _ => true

fun sum_cards(cs) =
    let fun f(cs, acc) =
	    case (cs) of
		[] => acc
	      | c::cs' => f(cs', acc + card_value c)
    in f(cs, 0)
    end

fun score(cs, goal) =
    let val sum = sum_cards(cs)
	val pre_score = if sum > goal
			then 3 * (sum - goal)
			else goal - sum
    in
	if all_same_color(cs)
	then pre_score div 2
	else pre_score
    end

fun officiate(cards, moves, goal) =
    let fun f(hcs, cs, ms) =
	(* hcs is held-cards, cs is card-list, ms is moves list*)
	    case (cs, ms) of
		(_, []) => score(hcs, goal)
	      | (cs', (Discard c)::ms') => f(remove_card(hcs, c, IllegalMove), cs', ms')
	      | ([], Draw::ms') => score(hcs, goal)
	      | (c::cs', Draw::ms') => if sum_cards(c::hcs) > goal
				       then score(c::hcs, goal)
				       else f(c::hcs, cs', ms')
    in
	f([], cards, moves)
    end

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1a *)
fun all_except_option(remove : string, items : string list) =
	case items of
	    [] => NONE
	  | item::items' => if same_string(remove, item)
				then SOME items'
             else case all_except_option(remove,items') of
                  NONE => NONE
                  | SOME l => SOME (item::l)

				(*
				NONE => if same_string(remove, item) then SOME items' else  NONE
			      | SOME lst => if same_string(remove, item) then SOME lst else SOME(item::lst)
						*)
	

(* 1b *)
fun get_substitutions1(substitutions : string list list, s : string) =
    case substitutions of
	[] => []
      | substitution::substitutions' => case all_except_option(s, substitution) of
					    NONE => get_substitutions1(substitutions', s)
					  | SOME lst => lst @ get_substitutions1(substitutions', s)
										
											       
(* 1c *)
fun get_substitutions2(substitutions : string list list, s : string) =
    let fun aux(subs : string list list, acc : string list) =
	    case subs of
		[] => acc
	      | sub::subs' => case all_except_option(s, sub) of
				 NONE => aux(subs', acc)
			       | SOME lst => aux(subs', acc @ lst)
    in
	aux(substitutions, [])
    end

(* 1d *)
fun similar_names(substitutions: string list list, full_name : {first:string,middle:string,last:string}) =
    let val {first=x, middle=y, last=z} = full_name
	fun aux(subs: string list) =
		case subs of
		    [] => []
		  | sub::subs' => {first=sub,middle=y,last=z}::aux(subs')
    in
	full_name::aux(get_substitutions2(substitutions, x))
    end
    
	 
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2a *)
fun card_color(c: card) =
    case c of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

(* 2b *)
fun card_value(c: card) =
    case c of
			  (_, Ace) => 11
      | (_, Num value) => value
			| _ => 10

(* 2c *)
fun remove_card(cs: card list, c: card, e: exn) =
    case cs of
	[] => raise e
      | card::cs' => if card = c then cs' else card::remove_card(cs', c, e)

(* 2d *)
fun all_same_color(cs: card list) =
    case cs of
	[] => true
      | c1::c2::cs' => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')
      | _ => true

(* 2e *)
fun sum_cards(cs: card list) =
    let fun aux(cards, acc) =
	    case cards of
		[] => acc
	      | card::cards' => aux(cards', acc + card_value(card))
    in
	aux(cs, 0)
    end
	
(* 2f *)
fun score(held_cards: card list, goal: int) =
    let val sum = sum_cards(held_cards) in
	let val prelim = if sum > goal then
			 3 * (sum - goal)
		     else
			 goal - sum
	in
	    if all_same_color(held_cards) then
		prelim div 2
	    else
		prelim
	end
    end

(* 2g *)
fun officiate(card_list: card list, move_list: move list, goal: int) =
    let fun aux(held_cards: card list, remaining_cards: card list, remaining_moves: move list) =
	    if sum_cards(held_cards) > goal then
		score(held_cards, goal)
	    else
		case remaining_moves of
		    [] => score(held_cards, goal)
		  | m::remaining_moves' =>
		    case m of
			Discard c => aux(remove_card(held_cards, c, IllegalMove), remaining_cards, remaining_moves')
		      | Draw => case remaining_cards of
				    [] => score(held_cards, goal)
				  | drawn::remaining_cards' =>
				    aux(drawn::held_cards, remove_card(remaining_cards, drawn, IllegalMove), remaining_moves')
    in
	aux([], card_list, move_list)
    end

(* 3a *)
fun score_challenge(held_cards: card list, goal: int) =
    
    let fun aux(cs: card list, g: int, same_color: bool) =
	    case cs of
		[] => let val prelim = if g < 0 then ~3 * g else g
		      in
			  if same_color then prelim div 2 else prelim
		      end
	      | c::cs' => case c of
			      (_, Ace) => Int.min(aux(cs', g - 1, same_color), aux(cs', g - 11, same_color))
			    | _ => aux(cs', g - card_value(c), same_color)
    in
	aux(held_cards, goal, all_same_color(held_cards))
    end

(* 3b *)
fun careful_player(card_list: card list, goal: int) =
    
      let fun reverse xs =
	    let fun aux(xs, acc)=
		    case xs of
			[] => acc
		      | x::xs' =>  aux(xs', x::acc)
	    in
		aux(xs, [])
	    end
		
	  fun aux (cs: card list, held_cards: card list, g: int, acc: move list) =
	      case cs of
		  [] => Draw::acc
		| c::cs' => if g = 0 then
				acc
			    else if g > 10 then (* Draw a card *)
				aux(cs', c::held_cards, g - card_value(c), Draw::acc)
			    else
				case held_cards of
				    [] => acc
				  | h::held_cards' =>


				    let val drop_strategy = aux(cs, held_cards', g + card_value(h), Discard(h)::acc) in
					if officiate(card_list,reverse(drop_strategy), goal) = 0 then
					    drop_strategy
					else
					    acc
				    end		   
				      
    in
	reverse(aux(card_list, [], goal, []))
    end
	

	    



	    
	 
	

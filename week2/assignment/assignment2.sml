(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, ls) =
   case ls of
   [] => NONE
   | x::xs => if same_string(x,s)
             then SOME xs
             else case all_except_option(s,xs) of
                  NONE => NONE
                  | SOME l => SOME (x::l)

(* [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]] *)
fun get_substitutions1(ls, s) =
   case ls of
      [] => []
      | x::xs => case all_except_option(s, x) of
               NONE => get_substitutions1(xs,s)
               | SOME e => e @ get_substitutions1(xs,s)

fun get_substitutions2(ls, s) =
   let fun keep(fs, ts) =
      case ts of
         [] => fs
         | x::xs => case all_except_option(s, x) of
                  NONE => keep(fs, xs)
                  | SOME e => keep(fs @ e, xs)
   in
      keep([], ls)
   end

   type full_name = {first  : string,
                  middle : string,
                  last   : string}

fun similar_names(ls, name: full_name) =
   let val pn = case name of {first=f, middle=_, last=_} => f
   val subs = get_substitutions2(ls, pn)
   fun get_names(ms, final) =
      case ms of
         [] => []
         | x::xs => case name of 
            {first=f, middle=m, last=l} => {first=x, middle=m, last=l} :: get_names(xs, final)
   in
      name :: get_names(subs, [name])
   end




             
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color(card: card) =
   case card of
      (suit, _) => if (suit = Clubs orelse suit = Spades)
                        then Black
                        else Red


fun card_value(card: card) =
   let val (suit, rank) = card
   in
      case rank of
         Ace => 11
         | Num i => i
         | _ => 10
   end


   
(*
fun remove_card(cs: card list, c: card, e: exn) =
   let fun discard(ls, acc) =
      x::xs => if x = c 
               then acc @ xs
               else
   in

   end


fun remove_card(cs: card list, c: card, e: exn) =
   let fun discard(cl, acc) =
      case cl of
         [] => acc
         | x::xs => if x = c
                  then acc @ xs
                  else discard(xs, acc @ [x])
   in
      discard(cs, [])
   end

*)
fun all_same_color(cs: card list) =
   case cs of
      [] => true
      | x::[] => true
      | x::y::xs => card_color(x) = card_color(y) = all_same_color(xs)

fun sum_cards(cs: card list) =
   let fun accumulator(cl, acc) =
      case cl of
         [] => acc
         | x::xs => accumulator(xs, acc + card_value(x))
   in
      accumulator(cs, 0)
   end

fun score(cs: card list, goal: int) =
   let val sum = sum_cards(cs)
   val areAllSameColor = all_same_color(cs)
   in
      let val preliminaryScore =
         if sum > goal
         then 3 * (sum - goal)
         else
         goal - sum
      in
         if areAllSameColor
         then preliminaryScore div 2
         else preliminaryScore
      end

   end
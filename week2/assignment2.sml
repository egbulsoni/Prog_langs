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

fun similar_names(ls, {first=a, middle=b, last=c}) =
   let val nicks = get_substitutions2(ls, a)
   in

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

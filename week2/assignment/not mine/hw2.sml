fun same_string (s1 : string, s2 : string) =
  s1 = s2

fun all_except_option (s, xs) =
  let
    fun aux (xs, found, acc) =
      case xs of
          [] => if found then SOME acc else NONE
        | x :: xs' =>
            let
              val same = same_string (x, s)
            in
              aux (xs', found orelse same, if same then acc else acc @ [x])
            end
  in
    aux (xs, false, [])
  end

fun get_substitutions1 (substitutions, s) =
  case substitutions of
      [] => []
    | sub :: subs =>
      case all_except_option (s, sub) of
          NONE => get_substitutions1 (subs, s)
        | SOME xs => xs @ get_substitutions1 (subs, s)

fun get_substitutions2 (substitutions, s) =
  let
    fun aux (substitutions, acc) =
      case substitutions of
          [] => acc
        | sub :: subs =>
          case all_except_option (s, sub) of
              NONE => aux (subs, acc)
            | SOME xs => aux (subs, acc @ xs)
  in
    aux (substitutions, [])
  end

fun similar_names (substitutions, { first = x, middle = y, last = z }) =
  let
    val subs = get_substitutions2 (substitutions, x)
    fun aux (subs, acc) =
      case subs of
          [] => acc
        | sub :: subs' => aux (subs', acc @ [{ first = sub, middle = y, last = z }])
  in
    aux (subs, [{ first = x, middle = y, last = z }])
  end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (s, r) =
  case s of
      Spades => Black
    | Clubs => Black
    | _ => Red

fun card_value (s, r) =
  case r of
      Num (n) => n
    | Ace => 11
    | _ => 10

fun remove_card (cs, c, e) =
  let
    fun aux (cs, found, acc) =
      case cs of
          [] => if found then acc else raise e
        | c' :: cs' =>
          if found
          then aux (cs', found, acc @ [c'])
          else
            let
              val same = c' = c
            in
              aux (cs', same, if same then acc else acc @ [c'])
            end
  in
    aux (cs, false, [])
  end

fun all_same_color (cs) =
  case cs of
      [] => true
    | _ :: [] => true
    | head :: (neck :: rest) => card_color head = card_color neck andalso all_same_color (neck :: rest)

fun sum_cards (cs) =
  let
    fun aux (cs, acc) =
      case cs of
        [] => acc
      | c :: cs' => aux (cs', acc + card_value c)
  in
    aux (cs, 0)
  end

fun score (cs, g) =
  let
    val sum = sum_cards cs
    val p = if sum > g then (sum - g) * 3 else g - sum
  in
    if all_same_color (cs)
    then p div 2
    else p
  end

fun officiate (cs, ms, g) =
  let
    fun aux (cs, ms, hcs) =
      case ms of
          [] => score (hcs, g)
        | m :: ms' =>
          case m of
              Discard c => aux (cs, ms', remove_card (hcs, c, IllegalMove))
            | Draw =>
              case cs of
                  [] => score (hcs, g)
                | c :: cs' =>
                  let
                    val hcs' = c :: hcs
                  in
                    if sum_cards hcs' > g
                    then score (hcs', g)
                    else aux (cs', ms', hcs')
                  end
  in
    aux (cs, ms, [])
  end

fun count_ace (cs) =
  let
    fun aux (cs, acc) =
      case cs of
          [] => acc
        | (_, r) :: cs' =>
          case r of
              Ace => aux (cs', acc + 1)
            | _ => aux (cs', acc)
  in
    aux (cs, 0)
  end

fun score_challenge (cs, g) =
  let
    fun enum_sums (sum, aces) =
      if aces < 0
      then []
      else (sum - aces * 10) :: enum_sums (sum, aces - 1)

    fun cal_p (sum) =
      if sum > g then (sum - g) * 3 else g - sum

    fun cal_best_p (sums) =
      case sums of
          [] => 0
        | sum :: [] => cal_p sum
        | head :: (neck :: rest) =>
          let
            val p = cal_p head
            val p' = cal_best_p (neck :: rest)
          in
            if p < p'
            then p
            else p'
          end

    val p = cal_best_p (enum_sums (sum_cards cs, count_ace cs))
  in
    if all_same_color (cs)
    then p div 2
    else p
  end

fun officiate_challenge (cs, ms, g) =
  let
    fun aux (cs, ms, hcs) =
      case ms of
          [] => score_challenge (hcs, g)
        | m :: ms' =>
          case m of
              Discard c => aux (cs, ms', remove_card (hcs, c, IllegalMove))
            | Draw =>
              case cs of
                  [] => score_challenge (hcs, g)
                | c :: cs' =>
                  let
                    val hcs' = c :: hcs
                  in
                    if sum_cards hcs' - count_ace hcs' * 10 > g
                    then score_challenge (hcs', g)
                    else aux (cs', ms', hcs')
                  end
  in
    aux (cs, ms, [])
  end

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, slist) =
    (* Helper recursion function. 
       "Forward" calls (decomposing the list) use a list as an argument, and "backward" calls use an option. *) 
    let fun all_except [] = NONE 
          | all_except (s'::slist) = if same_string(s, s') 
                                     then SOME slist 
                                     else case all_except(slist) of 
                                              NONE => NONE 
                                            | SOME slist' => SOME (s'::slist')
    in all_except(slist) end

fun get_substitutions1 (subs, s) = 
    case subs of 
        [] => []
           | slist::subs' => case all_except_option(s, slist) of 
                                NONE => get_substitutions1(subs', s)
                                     | SOME slist' => slist' @ get_substitutions1(subs', s)

fun get_substitutions2 (subs, s) =
    let fun get_sub(subs, acc) =
            case subs of 
                [] => acc
                   | slist::subs' => case all_except_option(s, slist) of 
                                           NONE => get_sub(subs', acc)
                                                | SOME slist' => get_sub(subs', acc @ slist') 
    in get_sub(subs, []) end

fun similar_names (subs, full_name) = 
    let val {first=name, middle=middle_name, last=last_name} = full_name
        val subs_for_name = get_substitutions2(subs, name)
        fun combine_names (subs) =
            case subs of 
                [] => []
             | s::subs' => {first=s, middle=middle_name,last=last_name}::combine_names(subs') 
    in full_name::combine_names(subs_for_name) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of 
        (Clubs, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red
     | (Spades, _) => Black

fun card_value card =
    case card of 
        (_, Num n) => n
     | (_, Ace) => 11
     | _ => 10 

fun remove_card (cs, c, e) =
    case cs of 
        [] => raise e
     | c'::cs' => if c' = c 
                  then cs' 
                  else c'::remove_card(cs', c, e)  

fun all_same_color (cs) =
    case cs of 
        [] => true
     | c::[] => true
     | c1::c2::cs' => card_color(c1) = card_color(c2) 
                      andalso all_same_color(c2::cs')  

fun sum_cards (cs) =
    let fun sum_cards_acc(cs, acc) =
            case cs of 
                [] => acc
             | c::cs' => sum_cards_acc(cs', acc + card_value(c))
    in sum_cards_acc(cs, 0) end

fun score (cs, goal) =
    let val sum = sum_cards(cs) 
        (* If sum is greater than goal, the preliminary score is three times (sum -  goal),
           else the preliminary score is (goal -  sum). *)
        val pre_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        (* If all the held-cards are the same color, the score is the preliminary score divided by 2 *)
        if all_same_color(cs) then pre_score div 2 else pre_score
    end

fun officiate (cs, mvs, goal) =
    let fun next_move (cs, held_cs, mvs) =
            case (cs, held_cs, mvs) of
                (* The game ends if there are no more moves. *)
                (_, _, []) => score(held_cs, goal)
              (* If the player discards some card c, play continues with the held-cards
                 not having c and the card-list unchanged. *)
              | (cs, held_cs, Discard c::mvs') => next_move(cs, remove_card(held_cs, c, IllegalMove), mvs')
              (* If the player draws and the card-list is (already) empty, the game is over. *)
              | ([], held_cs, Draw::mvs') => score(held_cs, goal)
              (* Else if drawing causes the sum of the held-cards to exceed the goal,
                 the game is over (after drawing). *)
              | (c::cs', held_cs, Draw::mvs') => if sum_cards(c::held_cs) > goal 
                                                 then score(c::held_cs, goal) 
              (* Else play continues with a larger held-cards and a smaller card-list. *)
                                                 else next_move(cs', c::held_cs, mvs') 
    (* The game starts with the held-cards being the empty list. *)
    in next_move(cs, [], mvs) end

(* challenge problems *)

fun change_card_rank (cs, from, to) =
    case cs of
        [] => NONE
      | (s, r)::cs' => if r = from 
                       then SOME((s, to)::cs')
                       else case change_card_rank(cs', from, to) of 
                                NONE => NONE
                             | SOME cs'' => SOME((s, r)::cs'')      

(* Remove first ace and add pseudo-ace card with Rank Num 1
   Return SOME cs' with pseudo-ace or NONE if there was no ace in cards *)
fun flip_ace (cs) = change_card_rank(cs, Ace, Num 1)

fun unflip_ace (cs) = change_card_rank(cs, Num 1, Ace)

fun score_challenge (cs, goal) =
    (* score with [unflipped] aces as 11 *)
    let 
        val pre_score = score (cs, goal)
        val new_score_opt = if sum_cards(cs) > goal
                            then flip_ace(cs)
                            else unflip_ace(cs)
    in 
        case new_score_opt of 
            NONE => pre_score
        (* found and flipped an ace *)
        |  SOME cs' => let val new_score = score(cs', goal) 
                       in 
                           if new_score > pre_score 
                           then pre_score 
                           else score_challenge(cs', goal)
                       end                     
    end

fun officiate_challenge (cs, mvs, goal) =   

    let 
        fun flip_ace_while_exceed(cs, goal) =
            if sum_cards(cs) <= goal
            then cs
            else case flip_ace(cs) of
                     NONE => cs
                   | SOME cs' => flip_ace_while_exceed(cs', goal) 

        fun next_move (cs, held_cs, mvs) =
            case (cs, held_cs, mvs) of
                (_, _, []) => score_challenge(held_cs, goal)
              | (cs, held_cs, Discard c::mvs') => next_move(cs, remove_card(held_cs, c, IllegalMove), mvs')
              | ([], held_cs, Draw::mvs') => score_challenge(held_cs, goal)
              | (c::cs', held_cs, Draw::mvs') => let val held_cs' = flip_ace_while_exceed(c::held_cs, goal)
                                                 in
                                                     if sum_cards(held_cs') > goal
                                                     then score_challenge(held_cs', goal)
                                                     else next_move(cs', held_cs', mvs')
                                                 end 
    in next_move(cs, [], mvs) end

fun careful_player (cs, goal) =

    let 
        fun remove_card_of_value(cs, value) =
            case cs of
                [] => NONE
              | c::cs' => if card_value(c) = value
                          then SOME (cs', c)
                          else
                              case remove_card_of_value(cs', value) of
                                   NONE => NONE
                                 | SOME (cs'', c') => SOME (c::cs'', c') 
                                

        fun get_moves(cs, held_cs) =
            if score(held_cs, goal) = 0 
            then []
            else 
                case cs of
                    [] => []
                 | c::cs' => let val value_to_remove = sum_cards(c::held_cs) - goal
                             in
                                 case remove_card_of_value(held_cs, value_to_remove) of 
                                     NONE => if goal - sum_cards(held_cs) > 10 
                                             then Draw::get_moves(cs', c::held_cs) 
                                             else []
                                   | SOME (cs'', c') => [(Discard c'), Draw]    
                             end            
    in get_moves(cs, []) end

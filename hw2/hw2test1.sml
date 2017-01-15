val test1 = all_except_option("string", ["string"]) = SOME []

(* TEST *)
val my_test1_1 = all_except_option("", []) = NONE
val my_test1_2 = all_except_option("wind", ["sun", "rain", "snow"]) = NONE 
val some_list1_3 = all_except_option("rain", ["sun", "rain", "snow"]) = SOME ["sun", "snow"]
(* /TEST *)

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []

(* TEST *)
val my_test2_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val my_test2_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
(* /TEST *)

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []

(* TEST *)
val my_test3_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val my_test3_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
(* /TEST *)

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
            [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
             {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

(* TEST *)
val my_test4_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"],["Elizaveta", "Yelisavet", "Elizabeth"]], {first="Elizabeth", middle="Petrovna", last="Romanova"}) =
            [{first="Elizabeth", middle="Petrovna", last="Romanova"}, {first="Betty", middle="Petrovna", last="Romanova"},
             {first="Elizaveta", middle="Petrovna", last="Romanova"}, {first="Yelisavet", middle="Petrovna", last="Romanova"}]
(* /TEST *)
                 
val test5 = card_color((Clubs, Num 2)) = Black

(* TEST *)
val my_test5_1 = card_color(Clubs, Num 9) = Black
val my_test5_2 = card_color(Diamonds, Jack) = Red
val my_test5_3 = card_color(Hearts, Queen) = Red
val my_test5_4 = card_color(Spades, Ace) = Black
(* /TEST *)

val test6 = card_value((Clubs, Num 2)) = 2

(* TEST *)
val my_test6_1 = card_value(Clubs, Num 9) = 9
val my_test6_2 = card_value(Diamonds, Jack) = 10
val my_test6_3 = card_value(Hearts, Queen) = 10
val my_test6_4 = card_value(Spades, Ace) = 11
(* /TEST *)

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

(* TEST *)
val my_test7_1 = remove_card([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace), (Hearts, Queen)], (Hearts, Queen), IllegalMove) = [(Clubs, Num 9), (Diamonds, Jack), (Spades, Ace), (Hearts, Queen)]
val my_test7_2 = (remove_card([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], (Hearts, King), IllegalMove); false) handle IllegalMove => true
(* /TEST *)

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

(* TEST *)
val my_test8_1 = all_same_color([(Clubs, Num 9), (Spades, Num 3),(Clubs, Jack),(Clubs, Num 5),(Spades, Queen)]) = true
val my_test8_2 = all_same_color([(Clubs, Num 9), (Spades, Num 3),(Hearts, Num 2),(Clubs, Num 5),(Spades, Queen)]) = false
(* /TEST *)

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

(* TEST *)
val my_test9_1 = sum_cards([(Clubs, Num 9), (Spades, Num 3),(Clubs, Jack),(Clubs, Num 5),(Spades, Queen)]) = 37
val my_test9_2 = sum_cards([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)]) = 40
(* /TEST *)

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

(* TEST *)
val my_test10_1 = score([(Clubs, Num 9), (Spades, Num 3),(Clubs, Jack),(Clubs, Num 5),(Spades, Queen)], 30) = 10
val my_test10_2 = score([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], 50) = 10
val my_test10_3 = score([(Hearts, Queen), (Spades, Ace), (Hearts, Queen)], 11) = 60
(* /TEST *)

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
            = 3 

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
                          
(* CHALLANGE TEST *)
val ch_test1_1 = score_challenge([(Clubs, Num 9), (Spades, Num 3),(Clubs, Jack),(Clubs, Num 5),(Spades, Queen)], 30) = 10
val ch_test1_2 = score_challenge([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], 50) = 10
val ch_test1_3 = score_challenge([(Hearts, Queen), (Spades, Ace), (Hearts, Ace), (Diamonds, Ace), (Hearts, Queen)], 33) = 0

val ch_test2_1 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4),(Spades, Ace), (Diamonds, Ace), (Spades, Num 2)],[Draw, Draw, Draw, Draw], 8) = 0

val ch_test2_2 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Hearts,Ace),(Diamonds,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42) = 6
             

val ch_test2_3 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) 

val ch_test3_1_1 = careful_player([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], 30)
val ch_test3_1_2 = officiate([(Clubs, Num 9), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], ch_test3_1_1, 30) = 0
                          
(* /CHALLANGE TEST *)

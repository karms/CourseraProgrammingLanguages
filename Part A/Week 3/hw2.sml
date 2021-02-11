(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (_, []) = NONE
  | all_except_option (opt, x::xs) = 
      if same_string(opt, x)
      then SOME(xs)
      else case all_except_option(opt, xs) of 
           NONE => NONE
         | SOME res => SOME(x::res)
    
fun get_substitutions1 (substitutions, s) = 
    case substitutions of
         [] => []
       | x::xs => case all_except_option(s, x) of
                       NONE => get_substitutions1(xs, s)
                     | SOME found => found @ get_substitutions1(xs, s)


fun get_substitutions2 (substitutions, s) = 
   let fun f (substitutions, acc) = 
          case substitutions of
               [] => acc
             | x::xs => case all_except_option(s, x) of
                             NONE => f(xs, acc)
                           | SOME found => found @ f(xs, acc)
   in 
      f(substitutions, [])
   end

fun similar_names (substitutions, original_name as {first:string, middle:string, last:string}) = 
    let fun walk subs =
            case subs of
                 [] => []
               | x::xs => {first = x, middle = middle, last = last} :: walk xs
    in 
       original_name :: walk (get_substitutions2(substitutions, first))
    end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (card) = 
    case card of
         (Clubs, _) => Black
       | (Spades ,_) => Black
       | (Diamonds, _) => Red
       | (Hearts, _) => Red

fun card_value (card) = 
    case card of
         (_, Ace) => 11
       | (_, Jack) => 10
       | (_, Queen) => 10
       | (_, King) => 10
       | (_, Num n) => n

fun remove_card (cs, c, e) = 
    case cs of
         [] => raise e
       | x::xs => if c = x 
                  then xs
                  else x :: remove_card(xs, c, e)

fun all_same_color (cs) = 
    case cs of
         [] => true
       | c::[]  => true 
       | h::n::rest => card_color(h) = card_color(n) andalso all_same_color(n::rest) 

fun sum_cards (cs) =
    let
      fun walk(xs, acc) =
          case xs of
               [] => acc
             | x::xs' => walk(xs', card_value(x) + acc)
    in 
        walk(cs, 0)
    end

fun score (hand, goal) = 
    let
        val sum = sum_cards(hand)
        val prelim = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color(hand) then prelim div 2 else prelim
    end

fun officiate (hand:card list, moves, goal) = 
    let
        fun run (hand, moves, held) = 
            if score(held, goal) > goal orelse hand = [] orelse moves = []
            then score(held, goal) 
            else
            case moves of
                 Draw::moves' => run(tl hand, moves', (hd hand)::held)
               | (Discard card)::moves' => run(hand, moves', remove_card(held, card, IllegalMove))
    in
        run(hand, moves, [])
    end


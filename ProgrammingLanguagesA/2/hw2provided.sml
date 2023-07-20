(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s : string, strlist : string list) =
    case strlist of
	[] => NONE
      | x::xs' => if same_string(s, x)
		  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE => NONE
			 | SOME y => SOME (x::y)

fun get_substitutions1 ( substitutions : string list list, s : string) =
    case substitutions of
	[] => []
      | x::xs' => case all_except_option(s, x) of
		      NONE => get_substitutions1(xs', s)
		    | SOME y => y @ get_substitutions1(xs', s)

fun get_substitutions2 (substitutions : string list list, s : string) =
    let
	fun aux (subs : string list list, acc : string list) =
	    case subs of
		[] => acc
	      | x::xs => case all_except_option(s, x) of
			      NONE => aux(xs, acc)
			    | SOME y => aux(xs, acc @ y)

    in
	aux(substitutions, [])
    end


fun similar_names (substitutions : string list list, name : {first:string,middle:string,last:string}) =
    let
	val {first=f, middle=m, last=l} = name
	fun make_names xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=m, last=l} :: (make_names xs')
    in
	name::make_names(get_substitutions2(substitutions, f))
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

fun card_color card =
    case card of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

fun card_value card =
    case card of
	(_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
      | (_,Num n) => n

fun remove_card (cs : card list, c : card, e) =
    case cs of
	[] => raise e
      | x::xs' => if x = c then xs' else x::remove_card(xs', c, e)

fun all_same_color (cs : card list) =
    case cs of
	[] => true
      | [_] => true
      | head::(neck::tail) => card_color head = card_color neck andalso all_same_color(neck::tail)

fun sum_cards (cs : card list) =
    let
	fun aux(cs : card list, acc : int) =
	    case cs of
		[] => acc
	      | c::cs' => aux(cs', card_value(c) + acc)
    in
	aux(cs, 0)
    end

fun score (cs : card list, goal : int) =
    let
	val sum = sum_cards(cs)
	val prelim = if sum >= goal then 3 * (sum - goal) else goal - sum
	val same = if all_same_color(cs) then 2 else 1
    in
	prelim div same
    end

fun officiate (cs : card list, ml: move list, goal: int) =
    let
	fun aux(current_cards, cards_left, moves_left) =
	    case moves_left of
		[] => score(current_cards, goal)
	      | (Discard c)::tail => aux(remove_card(current_cards, c, IllegalMove), cards_left, tail)
	      | Draw::tail => case cards_left of
				  [] => score(current_cards, goal)
				| c::rest => if sum_cards(c::current_cards) > goal
					     then score(c::current_cards, goal)
					     else aux(c :: current_cards, rest, tail)
    in
	aux([],cs,ml)
    end
	
    
    
			   
	

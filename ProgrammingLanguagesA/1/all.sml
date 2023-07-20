(* This is a comment *)

val x = 69;
val y = 31;
val z = (x + y) + (y + 2);
val q = z + 1;
val abs_of_z = if z < 0 then 0 - z else z;
val abs_of_z_simpler = abs z;


(* shadowing *)
val a = 10;
val b = a * 2;
val a = 5;
val c = b;
val d = a;
val a = a + 1;
val f = a * 2;

	   
(* functions *)
fun pow(x : int, y : int) =
    if y = 0
    then 1
    else x * pow(x, y-1)

fun cube(x : int) =
    pow(x,3)

val sixtyfour = cube(4)
val fortytwo = pow(2,2+2) + pow(4,2) + cube(2) + 2
						     
(* pairs and tuples *)
fun swap (pr : int * bool) =
    (# 2 pr, #1 pr)

fun sum_two_pairs (pr1 : int * int, pr2 : int * int) =
    (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod (x : int, y : int) =
    (x div y, x mod y)

fun sort_pair (pr: int * int) =
    if (#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr)

(* lists *)
val empty_list = []
val add_element = 3::empty_list
val is_empty = null empty_list
val first_element = hd [1,2,3]
val rest_of_list = tl [1,2,3]
val second_element = hd (tl [1,2,3])

fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

fun countdown (x : int) =
    if x=0
    then []
    else x::countdown(x-1)

fun append (xs : int list, ys: int list) =
    if null xs
    then ys
    else (hd xs) :: append((tl xs), ys)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else (#1 (hd xs)) :: firsts(tl xs)

fun seconds (xs : (int * int) list) =
    if null xs
    then []
    else (#2 (hd xs)) :: firsts(tl xs)

				      
fun sum_pair_list2 (xs : (int * int) list) =
    (sum_list (firsts xs)) + (sum_list (seconds xs))

(* let expressions *)
fun foo (z : int) =
    let
	val x = if z > 0 then z else 69
	val y = x + z + 9
    in
	if x > y then x * 3 else y * y
    end

fun bar () =
    let
	val x = 1
    in
	(let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
    end

	
(* Nested Functions *)
				 


fun countup_from1(x : int) =
    let
	fun count (from : int) =
	    if from = x
            then x ::[]
	    else from::count(from+1)
    in
	count(1)
    end

(* Options *)
val my_list = [1,2]
val some = SOME my_list
val is_some = isSome (SOME (hd my_list))
val val_of = valOf (SOME (hd my_list))
		   
fun max (xs : int list) =
    if null xs
    then NONE
    else
	let val tl_ans = max(tl xs)
	in if isSome tl_ans andalso valOf tl_ans > hd xs
	   then tl_ans
	   else SOME (hd xs)
	end



(* Booleans and Comparison Operations *)
val b1 = true andalso true;
val b2 = false orelse true;
val b3 = 4 >= 6;
val b4 = 5 = 6;
val b5 = 5 <> 6;
	    

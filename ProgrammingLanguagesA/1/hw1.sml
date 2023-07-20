
(* 1 *)
(* (int * int * int) * (int * int * int) --> bool *)
(* evaluates true if first argument is a date that comes before the second argument *)
	       
fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    (#1 d1) < (#1 d2) orelse
    ((#1 d1) = (#1 d2) andalso (#2 d1) < (#2 d2)) orelse
    ((#1 d1) = (#1 d2) andalso ((#2 d1) = (#2 d2)) andalso (#3 d1) < (#3 d2))

(* 2 *)
(* (int * int * int) list * int --> int *)
(* evaluates how many dates in the list are in the given month *)
	       
fun number_in_month (dates : (int * int * int) list, m : int) =
    if null dates
    then 0
    else
	let val tl_d = number_in_month(tl dates, m)
	in    
	    if (#2 (hd dates)) = m
	    then 1 + tl_d
	    else tl_d
	end
	    
(* 3 *)
(* (int * int * int) list * int list --> int *)
(* evaluates number of dates in the list of dates are in any of the months in the list of months *)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
(* (int * int * int) list * int --> (int * int * int) list *)
(* returns a list holding the dates from the argument list of dates that are in the month.  *)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	let
	    val tl_d = dates_in_month(tl dates, month)
	    val date = hd dates
	in    
	    if (#2 date) = month
	    then date :: tl_d
	    else tl_d
	end
    
				      
(* 5 *)
(* (int * int * int) list * int list --> (int * int * int) list *)
(* returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months *)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else
	let
	    fun append (xs : (int * int *int) list, ys : (int * int * int) list) =
		if null xs
		then ys
		else (hd xs) :: append(tl xs, ys)
	in
	    append(dates_in_month(dates, hd months), dates_in_months(dates, tl months))
	end

(* 6 *)
(* string list * int --> string *)
(* returns the nth element of the list where the head of the list is 1st *)

fun get_nth (strls : string list, n : int) =
    if n = 1
    then hd strls
    else get_nth(tl strls, n - 1)

(* 7 *)
(* (int * int * int) --> string *)
(* takes a date and returns a string of the form January 20, 2013 (for example). *)


		
fun date_to_string (date : (int * int * int)) =
    let val str_dates = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November", "December"]
    in get_nth(str_dates, (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
    end
 	
(* 8 *)
(* int * int list -> int *)
(*  return an int n such that the first n elements of the list add to less than sum, but the first 
    n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case  *)

fun number_before_reaching_sum ( sum : int, numls : int list) =
    if sum <= hd numls
    then 0
    else number_before_reaching_sum(sum - hd numls, tl numls) + 1


(* 9 *)
(* int --> int *)								    
(* returns what month that day is in (1 for January, 2 for February, etc.)*)

fun what_month (day : int) =
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, days_in_months) + 1
    end

(* 10 *)
(* int * int --> int list*)
(* takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] 
   where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2 *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)
					
			    

(* 11 *)
(* (int * int * int) list -> (int * int * int) option *)					
(* evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest (tl dates)
	in
	    if not(isSome tl_ans) orelse is_older(hd dates, valOf tl_ans)
	    then SOME (hd dates)
	    else tl_ans
	end
	    
		
					     
					

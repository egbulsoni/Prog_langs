
(* function 1 *)
fun is_older (d1 : int*int*int,d2 : int*int*int) =
    #1 d1 <= #1 d2 andalso #2 d1 <= #2 d2 andalso #3 d1 < #3 d2

(* function 2 *)	
fun number_in_month (date : (int*int*int) list, month : int) =
    if  null date 
    then 0
    else if #2 (hd date) = month
    then 1+ number_in_month(tl date,month)
    else  number_in_month(tl date,month)
	
(* function 3 *)	
fun number_in_months (date : (int*int*int) list, months : int list) =
    if null months
    then 0
    else let val count =  number_in_month(date,hd months)
	 in if count = 0
	    then number_in_months(date, tl months)
	    else count + number_in_months(date, tl months)
	 end
(* function 4 *)
fun dates_in_month ( date : (int*int*int) list, month  : int) =
    if null date
    then []
    else if #2 (hd date) = month
    then (hd date)::dates_in_month(tl date , month)
    else dates_in_month(tl date , month)
	     
			   
(* function 5 *)	
fun dates_in_months (date : (int*int*int) list, months : int list) =
    if null months
    then []
    else let val list = dates_in_month(date, hd months)
	 in if null list
	    then dates_in_months(date, tl months)
	    else list @ dates_in_months(date, tl months)
	 end
	     
(* function 6 *)
fun get_nth (str_list : string list, i : int) =
    if i = 1 
    then hd str_list
    else  get_nth(tl str_list,i-1)

(* function 7 *)
fun date_to_string (date : int*int*int) =
    let val months = ["January","February","March","April","May","June","July",
		      "August","September","October","November","December"]
    in
       get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end


(*function 8 *)
fun number_before_reaching_sum (sum : int, int_list : int list) =
    if sum <= hd int_list
    then 0
    else  number_before_reaching_sum(sum - (hd int_list), tl int_list)+1

				       
(* function 9 *)
fun what_month (day : int(* from 1 to 365*)) =
    let val list = [ 31, 29, 30, 31, 30, 31, 30, 31, 30, 31, 30, 31]
    in
       number_before_reaching_sum(day, list)+1
    end
	
				       
(* function 10 *)
fun month_range (day1 : int , day2 : int) =
    if day1 = day2
    then what_month(day2)::[]
    else if day1 < day2
    then what_month(day1)::month_range(day1 + 1, day2)
    else []


(* function 11 *)

fun oldest (dates_list : (int*int*int) list) =
    if null dates_list
    then NONE 
    else let fun max (dates_list : (int*int*int) list) =
		 if null (tl dates_list)
		 then hd dates_list
		 else let val tl_ans = max(tl dates_list)
		      in
                if is_older(hd dates_list , tl_ans)
                then hd dates_list
                else tl_ans
		      end
		 in
		     SOME (max dates_list)
		 end

		 
	
		  
		 
	     
	     
			  
	
	   
	
	

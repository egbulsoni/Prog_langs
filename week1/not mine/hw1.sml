fun is_older (date1: int*int*int, date2: int*int*int)=
    if (#1 date1)<(#1 date2)
    then true
    else if (#1 date1)= (#1 date2) andalso (#2 date1)<(#2 date2)
    then true
    else if (#2 date1)=(#2 date2) andalso (#3 date1)<(#3 date2)
    then true
    else false
	     
fun number_in_month (dates: (int*int*int) list, month: int)  =
    let
      fun count (x: int , dates: (int*int*int) list) =
          if null (tl dates)
          then 
              if #2(hd dates) <> month
                      then x
                     else x+1
         else if #2(hd dates) = month
            then count (x+1, tl dates)
            else count (x, tl dates)
    in
          count (0, dates)
    end
 

fun number_in_months (dates: (int*int*int) list, months: int list) =
    let
	fun counts (x : int, months : int list) =
	    if null (tl months)
	    then x+number_in_month (dates, hd months)			
	    else counts (x+(number_in_month (dates,hd months)), tl months)
    in
	counts (0, months)
    end

fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates then []
    else if (#2 (hd dates)) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)   

	
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null (months)
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
							   
fun get_nth (words: string list, n : int) =
    if n = 1
    then hd words
    else get_nth (tl words, n - 1)

fun date_to_string (date: int*int*int) =
    get_nth(["January","February","March","April","May","June", "July", "August", "September", "October", "November", "December"], #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
																						       
fun number_before_reaching_sum (sum : int, number : int list) =
    if hd number >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd number, tl number)
				       
    
fun what_month ( day : int) =
    let val  month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, month)+ 1 
				  
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1):: month_range(day1 + 1, day2)
				      
fun oldest ( dates : (int*int*int) list) =
    if null dates then NONE
    else
	let
	    val tl_ans=oldest(tl dates)
	in
	    if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
	    else SOME (hd dates)
	end

fun delete ( i : int, elements : int list)=
    if null elements then []
    else
	if i = hd elements then delete(i, tl elements)
	else hd elements :: delete(i, tl elements)

fun remove_duplicate (months : int list) =
    if null months then []
    else hd months :: remove_duplicate(delete(hd months, tl months))
				      			  
	    
fun number_in_months_challenge (dates: (int*int*int) list, months: int list)  =
    number_in_months(dates, remove_duplicate(months))

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list)=
    dates_in_months(dates, remove_duplicate(months)) 
    

fun reasonable_date (date : int*int*int) =
    (* positive year <0*)
    if (#1 date)<= 0 then false
    else
	  (*month between 1 and 12*)
	if (#2 date) >= 1 andalso (#2 date) <= 12
	then
	    let
		val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		fun get_nth(ints: int list, n:int)=
		    if n=1 then hd ints
		    else get_nth(tl ints, n-1)
				
		fun is_leap (year:int)=
		    year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <>0
	    in
		if is_leap(#1 date) andalso (#3 date) <= get_nth(days_in_month_leap, #2 date)
		then true
		else
		    if (#3 date) <= get_nth(days_in_month, #2 date)
		    then true
		    else false 

			 
	    end
										      
	else false
    

fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if (#1 date1 ) <> (#1 date2)
  then (#1 date1) < (#1 date2)
  else if (#2 date1) <> (#2 date2)
  then (#2 date1) < (#2 date2)
  else (#3 date1) < (#3 date2) 
  		     

fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
       then 1 + number_in_month(tl dates, month)
       else number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)



fun dates_in_month(dates : (int * int * int) list, month : int) =
  let
    fun check_date(date : (int * int * int)) = 
      if #2 date = month
      then true
      else false
  in
    if null dates
    then []
    else if check_date(hd dates)
    then hd dates::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
  end
    

fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(data: string list, n: int) =
  if n = 1
  then hd(data)
  else get_nth(tl data, n-1)

fun date_to_string (date : int * int * int) =
  let 
      val names = ["January ", "February ", "March ", "April ", "May ", "June ",
                    "July ", "August ", "September ", "October ", "November ", "December "]
  in
    get_nth(names,#2 date) ^ Int.toString(#3 date) 
    ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(given_sum : int, data : int list) =
  if hd data < given_sum
  then 1 + number_before_reaching_sum(given_sum - hd data, tl data)
  else 0

fun what_month(day : int) =
  let
      val days_number = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, days_number) + 1
  end

fun month_range(day1 : int, day2 : int)=
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else
      let 
          val tl_oldest = oldest(tl dates)
      in
          if (isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates))
          then tl_oldest
          else SOME (hd dates)
      end
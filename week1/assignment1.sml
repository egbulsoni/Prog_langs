fun is_older (d1: int * int * int, d2: int * int * int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2
    then true
    else if #1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2
    then true
    else false


fun number_in_month (ds: (int * int * int) list, m: int) =
    let fun month_counter (c: int, xs: (int * int * int) list) =
        if null xs
        then c
        else if #2 (hd xs) = m
        then month_counter(c + 1, tl xs)
        else month_counter(c, tl xs)
    in
        month_counter (0, ds)
    end

fun number_in_months (ds: (int * int * int) list, ms: int list) =
    let fun month_iter (i: int, ms: int list) =
        if null ms
        then i
        else
        month_iter(i + number_in_month(ds, hd ms), tl ms)
    in
        month_iter(0, ms)
    end

fun dates_in_month (ds: (int * int * int) list, m: int) =
    let fun date_iter (xs: (int * int * int) list, matching_dates: (int * int * int) list) =                    
        if null xs
        then matching_dates
        else if #2 (hd xs) = m            
        then date_iter(tl xs, hd xs :: matching_dates)
        else date_iter(tl xs, matching_dates)    
    in        
        date_iter(ds, [])
    end

fun dates_in_months(ds: (int * int * int) list, ms: int list) =
    let fun month_iter(final_ds: (int * int * int) list, ms: int list) =
        if null ms
        then final_ds
        else
        month_iter(final_ds @ dates_in_month(ds, hd ms), tl ms)
    in
        month_iter([], ms)
    end


fun get_nth(words: string list, n: int) =
    if n < 1
    then ""
    else if n = 1
    then hd words
    else
    get_nth(tl words, n -1)

fun date_to_string(date: (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, nums: int list) =
    let fun reduce(current_sum: int, ns: int list, pos: int) =
        if current_sum + hd ns >= sum
        then pos
        else
        reduce(current_sum + hd ns, tl ns, pos + 1)
    in
        reduce(0, nums, 0)
    end

fun what_month(yd: int) =
    ceil (real(yd) / real(31))

fun month_range(yd1, yd2) =
    let fun date_iter(cur: int, month_list: int list) =
        if cur = yd2
        then month_list @ [what_month(cur)]
        else
        date_iter(cur + 1, month_list @ [what_month(cur)])
    in
        date_iter(yd1, [])
    end

fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else let fun max_value(ds: (int * int * int) list) =
            if null (tl ds)
            then hd ds
            else let val tl_ans = max_value(tl ds)
            in
                if is_older(hd ds, tl_ans)
                then hd ds
                else tl_ans
            end
        in
            SOME (max_value dates)
        end
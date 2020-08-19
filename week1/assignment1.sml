fun is_older (d1: int * int * int, d2: int * int * int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2
    then true
    else if #1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2
    then true
    else false


fun number_in_month (xs: (int * int * int) list, m: int) =
    let fun month_counter (c: int, xs: (int * int * int) list) =
        if null xs
        then c
        else if #2 (hd xs) = m
        then month_counter(c + 1, tl xs)
        else month_counter(c, tl xs)
    in
        month_counter (0, xs)
    end

fun number_in_months (xs: (int * int * int) list, ms: int list) =
    let fun month_iter (i: int, ms: int list) =
        if null ms
        then i
        else
        month_iter(i + number_in_month(xs, hd ms), tl ms)
    in
        month_iter(0, ms)
    end

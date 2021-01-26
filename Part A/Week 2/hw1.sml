fun is_older ((year1 : int, month1 : int, day1 : int), (year2 : int, month2 : int, day2 : int)) =
    if year1 > year2 then false else 
    if year1 < year2 then true else 
    if month1 > month2 then false else 
    if month1 < month2 then true else 
    if day1 > day2 then false else
    if day1 < day2 then true else false;

fun month_in_date ((year, month, day), selected_month : int) =
    if month = selected_month then 1 else 0

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0 else
        month_in_date(hd dates, month) + 
        number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null dates then 0 else
    if null months then 0 else
        number_in_month(dates, hd months) + 
        number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) = 
    if null dates then []
    else if month_in_date(hd dates, month) = 1 
         then hd dates :: dates_in_month(tl dates, month)
         else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) = 
    if null dates then [] else
    if null months then [] else
        dates_in_month(dates, hd months) @
        dates_in_months(dates, tl months)

fun get_nth (words : string list, nth) =
     if nth < 1 then "" else
     if null words then "" else
     if nth = 1 then (hd words) else
     get_nth (tl words, nth - 1)

fun date_to_string (year : int, month : int, day : int) = 
    get_nth(["January", "February", "March", "April", "May", 
    "June", "July", "August", "September", "October", 
    "November", "December"], month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)

fun number_before_reaching_sum (sum, all : int list) =
    let 
        fun walk (amount, count, rest : int list) =
            if null rest 
            then count 
            else
                let 
                    val current = amount + hd rest 
                in 
                    if current >= sum 
                    then count 
                    else walk (current, count + 1, tl rest)
                end 
    in
        walk (0, 0, all)
    end


fun what_month (day : int) =
    1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

fun month_range (day1 : int, day2 : int) =
            if day1 > day2
            then []
            else (what_month day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    let 
        fun walk (rest, oldest) =
            if null rest 
            then SOME oldest
            else if is_older (oldest, hd rest)
                 then walk (tl rest, oldest)
                 else walk (tl rest, hd rest)
    in 
        if null dates
        then NONE
        else walk (tl dates, hd dates)
    end
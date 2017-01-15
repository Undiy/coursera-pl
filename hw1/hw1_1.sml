(* 1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let 
        val year1 = #1 date1
        val month1 = #2 date1
        val day1 = #3 date1
                  
        val year2 = #1 date2
        val month2 = #2 date2
        val day2 = #3 date2
    in
        if year1 <> year2
        then year1 < year2
        else
            if month1 <> month2
            then month1 < month2
            else day1 < day2
    end
        
(* 2 *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
    if null dates 
    then 0
    else let val number_tl = number_in_month(tl dates, month) 
        in
            if #2 (hd dates) = month then number_tl + 1 else number_tl
        end

(* 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) = 
    if null dates orelse null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) = 
    if null dates 
    then []
    else let 
            val dates_tl = dates_in_month(tl dates, month) 
            val date = hd dates 
        in
            if #2 date = month then date :: dates_tl  else dates_tl
        end

(* 5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) = 
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 *)
fun get_nth (strings : string list, n : int) =
    if n = 1 
    then hd strings
    else get_nth(tl strings, n-1)

(* 7 *)
fun date_to_string (date : int * int * int) = 
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end
(* 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        fun get_number (sum : int, xs : int list, number : int) = 
            if null xs orelse hd xs >= sum 
            then number         
            else get_number(sum - hd xs, tl xs, number + 1)
    in
        get_number(sum, xs, 0)
    end

(* 9 *)
fun what_month (day_of_year : int) = 
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, days_in_months) + 1
    end

(* 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE   
    else let
            fun oldest_nonempty(dates : (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else let val oldest_tl = oldest_nonempty(tl dates)
                    in
                        if is_older(oldest_tl, hd dates) 
                        then oldest_tl 
                        else hd dates
                    end
        in
            SOME (oldest_nonempty dates)
        end

(* 12 *)

fun months_without_dups (months : int list) = 
    let
        fun contains_month (months : int list, month) = 
            not (null months) andalso
            (hd months = month orelse contains_month(tl months, month))
        
        fun make_new_months (month : int) = 
            if month > 12
            then []
            else 
                if contains_month(months, month)
                then month :: make_new_months(month + 1) 
                else make_new_months(month + 1)

    in
        make_new_months(1)
    end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    number_in_months(dates, months_without_dups(months))

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    dates_in_months(dates, months_without_dups(months))

(* 13 *)
fun reasonable_date (date : int * int * int) = 
    let
        fun is_leap_year (year : int) = 
            year mod 4 = 0 andalso
            (year mod 100 <> 0 orelse year mod 400 = 0) 
        
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        fun get_nth_int (xs : int list, n : int) =
            if n = 1 
            then hd xs
            else get_nth_int(tl xs, n-1)

        val year = #1 date
        val month = #2 date
        val day = #3 date
    in
        year >= 1 andalso
        month >= 1 andalso month <= 12 andalso
        day >= 1 andalso
        if is_leap_year(year) andalso month = 2
        then day <= 29
        else day <= get_nth_int(days_in_months, month)
    end

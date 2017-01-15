val test1 = is_older((2012,3,1),(2012,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1),(1999,10,5),(1920,2,11),(1765,3,15),(2000,2,2)],2) = 3

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(1999,10,5),(1920,2,11),(1765,3,15),(2000,2,2)],[2,3,4]) = 6

val test4 = dates_in_month([(2012,2,28),(2013,12,1),(1999,10,5),(1920,2,11),(1765,3,15),(2000,2,2)],2) = [(2012,2,28),(1920,2,11),(2000,2,2)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((1905, 9, 31)) = "September 31, 1905"

val test8 = number_before_reaching_sum(10, [11,2,3,4,5]) = 0

val test9 = what_month(60) = 3 andalso what_month(59) = 2

val test10 = month_range(57, 61) = [2,2,2,3,3]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12_1 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(1999,10,5),(1920,2,11),(1765,3,15),(2000,2,2)],[2,2,2,3,4,2,3,4,2]) = 6
val test12_2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,3,4,2,3,4,2]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test13 = reasonable_date((0, 10, 10)) = false andalso reasonable_date((1999, 2, 29)) = false andalso reasonable_date((1900, 2, 29)) = false andalso reasonable_date((2004, 2, 29)) = true

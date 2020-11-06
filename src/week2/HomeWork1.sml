val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun is_older (a: int*int*int, b: int*int*int) =
  if #1 a > #1 b
  then
    false
  else
    if #1 a < #1 b
    then
      true
      else
        if #2 a > #2 b
        then
          false
        else
          if #2 a < #2 b
          then
            true
          else
            if #3 a >= #3 b
            then
              false
            else
              true

fun number_in_month (given_months: (int*int*int) list, month: int) =
  let
    fun num_month_iter (given_months: (int*int*int) list, month: int) =
      if given_months = []
      then 0
      else
        let
          val first = hd given_months
        in
          if #2 first = month
          then
            1 + num_month_iter (tl given_months, month)
          else
            num_month_iter (tl given_months, month)
        end
  in
    num_month_iter (given_months, month)
  end

fun number_in_months (given_months: (int*int*int) list, months: int list) =
  if months = []
  then 0
  else
    number_in_month (given_months, hd months) + number_in_months(given_months,
    tl months)

fun dates_in_month (given_months: (int*int*int) list, month: int) =
  let
    fun date_month_iter (given_months: (int*int*int) list, month: int) =
      if given_months = []
      then []
      else
        let
          val head = hd given_months
        in
          if #2 head = month
          then
            [ head ] @ date_month_iter (tl given_months, month)
          else
            date_month_iter (tl given_months, month)
        end
  in
    date_month_iter (given_months, month)
  end

fun dates_in_months (given_months: (int*int*int) list, months: int list) =
  if months = []
  then []
  else dates_in_month (given_months, hd months) @ dates_in_months(given_months,
  tl months)

fun get_nth(given: string list, index: int) =
  if index = 1
  then hd given
  else get_nth (tl given, index - 1)

val months_ = ["January", "February", "March", "April", "May", "June", "July",
 "Autust", "September", "October", "November", "December"]

fun get_month_string (month: int) =
  let
    fun get_month_iter (months: string list, month: int) =
      if month = 1
      then hd months
      else get_month_iter (tl months, month - 1)
  in
    get_month_iter (months_, month)
  end

fun date_to_string (y: int, m: int, d: int) =
  get_month_string (m) ^ " " ^ Int.toString (d) ^ ", " ^ Int.toString(y)

fun number_before_reaching_sum (sum: int, given: int list) =
  let
    fun num_before_iter (sum_: int, given: int list, current: int) =
      if current < sum_ andalso current + hd (given) >= sum_
      then 0
      else
        1 + num_before_iter (sum_, tl given, current + hd (given))
  in
    num_before_iter (sum, given, 0)
  end

fun what_month (day: int) =
  let
    fun what_month_iter (day_: int, days: int list) =
      if day_ <= 0
      then 0
      else 1 + what_month_iter (day_ - hd (days), tl days)
  in
    what_month_iter (day, months)
  end

fun month_range (start: int, end_: int) =
  let
    fun generator (current: int, end__: int) =
      if current > end__
      then []
      else [ what_month (current) ] @ generator (current + 1, end__)
  in
    generator (start, end_)
  end

fun oldest (given: (int*int*int) list) =
  let
    fun oldest_iter (current_oldest: int*int*int, given_: (int*int*int) list) =
      if given_ = []
      then SOME current_oldest
      else
        if is_older (hd given_, current_oldest)
        then oldest_iter (hd given_, tl given_)
        else oldest_iter (current_oldest, tl given_)
  in
    if given = []
    then NONE
    else
      oldest_iter (hd given, tl given)
  end

let date_fun d m =
    match m with
        | "Jan" | "Mar" | "May" | "Jul" | 
          "Aug" | "Oct" | "Dec" -> 
            if d > 0 && d < 32 then true else false
        | "Apr" | "Jun" | "Sep" | "Nov" ->
            if d > 0 && d < 31 then true else false
        | "Feb" ->
            if d > 0 && d < 29 then true else false
        | _ -> false

let rec fib x = 
    if x = 0 || x = 1 then x
    else fib (x - 1) + fib (x - 2)

let rec fib_fast_helper p pp n =
    if n = 1 then p
    else fib_fast_helper (p + pp) p (n - 1)

let rec fib_fast n = fib_fast_helper 1 0 n


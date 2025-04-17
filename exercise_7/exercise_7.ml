(*** Exercise: init matrix ***)
let init_matrix x y f =
    let default = f 0 0 in
    let matrix = Array.make_matrix x y default in
    for i = 0 to x - 1 do
        matrix.(i) <- Array.init y (f i)
    done;
    matrix

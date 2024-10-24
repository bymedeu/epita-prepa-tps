open Z


let rec break_rec key num =
    if key mod num = zero then
        (num, key / num)
    else
        break_rec key (num - one)

let break key =
    let (a, _) = key in
    let sqrt_a = sqrt a in
    if sqrt_a mod (one + one) = one then
        break_rec a (sqrt_a - one)
    else
        break_rec a sqrt_a

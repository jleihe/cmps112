open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
    let listzero = []
    let listone = 1 :: []
    let listtwo = 2 :: []
    
    (*Compose*)
    let compose f g x = f (g x)
    (*Even?*)
    let even (x: int list) = match x with
        | [] -> false
        | x -> car x mod 2 = 0
    (*Odd*)
    let odd = compose not even
    
    (*Trim leading zeros*)
    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]     -> []
            | car::cdr ->
                 let cdr' = trimzeros' cdr
                 in  match car, cdr' with
                     | 0, [] -> []
                     | car, cdr' -> car::cdr'
    in trimzeros' list

    (*Convert String to Charlist*)
    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []
    
    (*Convert String to Bigint*)
    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)
    
    (*Convert Bigint to String*)
    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (*List1 > list2 (true/false)?*)
    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []              -> 0
        | list1, []         -> 1
        | [], list2         -> -1
        | list1, list2    -> 
            let res = cmp' (cdr list1) (cdr list2)
            in if res != 0 then res else 
                if (car list1) < (car list2) then -1 else
                    if (car list1) > (car list2) then 1 else 0
    
    (*Compare two numbers*)
    let cmp (Bigint (neg1, list1)) (Bigint (neg2, list2)) = 
        cmp' list1 list2

    (*add' is a helper function for add*)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  trimzeros (sum mod radix :: add' cdr1 cdr2 (sum / radix))
    
    (*sub' is a helper function for sub*)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let is_carry = (car1 - car2 - carry) < 0
            in let sum = (
                if is_carry then (car1 + 10 - car2 - carry) 
                else (car1 - car2 - carry))
            in  trimzeros 
            (sum :: sub' cdr1 cdr2 (if is_carry then 1 else 0))

    (*Add two bigints*)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let res = cmp (Bigint (neg1, value1)) (Bigint (neg2, value2));
        in if res = 0 then
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Pos, add' value1 value2 0)
                    | Pos, Neg   -> zero
                    | Neg, Pos   -> zero
                    | Neg, Neg  -> Bigint (Neg, add' value1 value2 0)
            else if res = 1 then
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Pos, add' value1 value2 0)
                    | Pos, Neg   -> Bigint (Pos, sub' value1 value2 0)
                    | Neg, Pos   -> Bigint (Neg, sub' value1 value2 0)
                    | Neg, Neg  -> Bigint (Neg, add' value1 value2 0)
            else
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Pos, add' value2 value1 0)
                    | Pos, Neg   -> Bigint (Neg, sub' value2 value1 0)
                    | Neg, Pos   -> Bigint (Pos, sub' value2 value1 0)
                    | Neg, Neg  -> Bigint (Neg, add' value2 value1 0)
    
    (*Subtract two bigints*)
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let res = cmp (Bigint (neg1, value1)) (Bigint (neg2, value2));
        in if res = 0 then
                match (neg1, neg2) with
                    | Pos, Pos    -> zero
                    | Pos, Neg   -> Bigint (Pos, add' value1 value2 0)
                    | Neg, Pos   -> Bigint (Neg, add' value1 value2 0)
                    | Neg, Neg  -> zero
            else if res = 1 then
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Pos, sub' value1 value2 0)
                    | Pos, Neg   -> Bigint (Pos, add' value1 value2 0)
                    | Neg, Pos   -> Bigint (Neg, add' value1 value2 0)
                    | Neg, Neg  -> Bigint (Neg, sub' value1 value2 0)
            else
                match (neg1, neg2) with
                    | Pos, Pos   -> Bigint (Neg, sub' value2 value1 0)
                    | Pos, Neg   -> Bigint (Pos, add' value2 value1 0)
                    | Neg, Pos   -> Bigint (Neg, add' value2 value1 0)
                    | Neg, Neg  -> Bigint (Pos, sub' value2 value1 0)
    
    (*Double the given value*)
    let double number = add' number number 0
    
    (*mul' is a helper function for mul*)
    let rec mul' (multiplier, powerof2, multiplicand') =
        if cmp' powerof2 multiplier = 1
        then multiplier, listzero
        else let remainder, product =
                mul' (multiplier,  double powerof2, 
                    double multiplicand')
            in  if cmp' remainder powerof2 = -1
                then remainder, product
                else (sub' remainder powerof2 0), 
                    (add' product multiplicand' 0)
                    
    (*Multiply multiplier by multiplicand*)
    let mul (Bigint (neg1, mPlier)) (Bigint (neg2, mPlicand)) =
        let neg = match (neg1, neg2) with
            | neg1, neg2 when neg1 = neg2 -> Pos
            | neg1, neg2 -> Neg
        in let _, product = mul' (mPlier,  listone, mPlicand)
        in Bigint(neg, product)
        
    (*divrem' is a help function for divrem*)
    let rec divrem' (dividend, powerof2, divisor') =
        if cmp' divisor' dividend = 1
        then listzero, dividend
        else let quotient, remainder =
                 divrem' (dividend, double powerof2, double divisor')
             in  if cmp' remainder divisor' = -1
                 then quotient, remainder
                 else (add' quotient powerof2 0), 
                    (sub' remainder divisor' 0)
                 
    (*divrem is a helper function for div and rem*)
    let divrem (dividend, divisor') = 
        divrem' (dividend, listone, divisor')
        
    (*Divide dividend by divisor*)
    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let neg = match (neg1, neg2) with
            | neg1, neg2 when neg1 = neg2 -> Pos
            | neg1, neg2 -> Neg
        in let quotient, _ = divrem (dividend, divisor)
        in Bigint(neg, quotient)
        
    (*The remainder after dividing dividend by divisor*)
    let rem (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let neg = match (neg1, neg2) with
            | neg1, neg2 when neg1 = neg2 -> Pos
            | neg1, neg2 -> Neg
        in let _, remainder = divrem (dividend, divisor)
        in Bigint(neg, remainder)
    
    
    (*helper div*)
    let hDiv  (dividend, divisor') = 
        let res, _ = divrem' (dividend, listone, divisor')
        in res
        
   (*helper mul*)
    let hMul (mPlier, mPlicand) =
        let _, product = mul' (mPlier,  listone, mPlicand)
        in product
        
    (*Double*)
    let double var = 
        let _, res = mul' (var, listone, var)
        in res
       
       
       let rec pow' (base: int list) (expt: int list) 
            (result: int list) = 
            match expt with
                | [] -> result
                | expt  -> 
                    let a = sub' expt listone 0
                    in let _, b = mul' (result, listone, base)
                    in pow' base a b
        
        
        (*(result: int list) = match expt with
        | [0] -> result
        | expt when odd expt -> 
            let a = sub' expt listone 0;
            in let _, b = mul' (result, listone, base)
            in pow' base a b
        | expt -> 
            let a = double base
            in let _, b = divrem' (expt, listone, listtwo)
            in pow' a b result*)
            
    let pow  (Bigint (neg1, base)) (Bigint (neg2, expt)) =
        if neg2 = Pos then 
            let res = pow' base expt listone
            in Bigint (neg1, res)
        else
            Bigint (neg1, listzero)

            
end


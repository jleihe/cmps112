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
    
    (*Compose*)
    let compose f g x = f (g x)
    (*Even?*)
    let even x = x mod 2 = 0
    (*Odd*)
    let odd = compose not even
    
    (*Trim leading zeros*)
    let trimzeros list =
    let rec trimzeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
             let cdr' = trimzeros' cdr
             in  match car, cdr' with
                 | 0, [] -> []
                 | car, cdr' -> car::cdr'
    in trimzeros' list
    ;;

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

    (*Lists > list2 (true/false)?*)
    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []             -> 0
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
          in let sum = (if is_carry then (car1 + 10 - car2 - carry) else (car1 - car2 - carry))
          in  trimzeros (sum :: sub' cdr1 cdr2 (if is_carry then 1 else 0))

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
                    | Neg, Neg  -> Bigint (Neg, add' value1 value2 0)
            else if res = 1 then
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Pos, sub' value1 value2 0)
                    | Pos, Neg   -> Bigint (Pos, add' value1 value2 0)
                    | Neg, Pos   -> Bigint (Neg, sub' value1 value2 0)
                    | Neg, Neg  -> Bigint (Neg, add' value1 value2 0)
            else
                match (neg1, neg2) with
                    | Pos, Pos    -> Bigint (Neg, sub' value2 value1 0)
                    | Pos, Neg   -> Bigint (Neg, add' value2 value1 0)
                    | Neg, Pos   -> Bigint (Pos, add' value2 value1 0)
                    | Neg, Neg  -> Bigint (Pos, add' value2 value1 0)

    let mul = add

    let div = add

    let rem = add

    let pow = add
    (*
    let rec power' (base, expt, result) = match expt with
    | 0                   -> result
    | expt when even expt -> power' (base *. base, expt / 2, result)
    | expt                -> power' (base, expt - 1, base *. result)

    let power (base, expt) =
        if expt < 0 then power' (1. /. base, - expt, 1.)
        else power' (base, expt, 1.)
    *)
    let lengthrec list = (*deprecate?*)
        let rec lengthrec' list' len' = match list' with
            | [] -> len'
            | _::cdr -> lengthrec' cdr (len' + 1)
        in  lengthrec' list 0

end


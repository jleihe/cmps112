(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

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

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

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

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (*Lists > list2 (true/false)?*)
    let rec lessThan' list1 list2 result = match (list1, list2) with
        | list1, []     -> true
        | [], list2     -> false
        | car1::cdr1, car2::cdr2 -> 
            let result = (car1 < car2)
            in (if (lessThan' cdr1 cdr2 result) = true 
            then true else result)
            
    let lessThan List(list1) List(list2) =
        lessThan' list1 list2 false

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero
    
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: sub' cdr1 cdr2 (sum / radix)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if lessThan [9, 1] [1]
        then Bigint(neg1, [1])
        else zero

    let mul = add

    let div = add

    let rem = add

    let pow = add
    (*let odd n = n mod 2 <> 0;;
    let pow (Bigint (neg1, a)) (Bigint (neg2, n)) =
        let rec powert' a n result = match n with
            | 0            -> result
            | n when odd n -> powert' a (n - 1) (result *. a)
            | n            -> powert' (a *. a) (n / 2) result
        in  if n < 0 then powert' (1. /. a) (- n) 1.
                     else powert' a n 1.
        ;;*)
    let lengthrec list =
        let rec lengthrec' list' len' = match list' with
            | [] -> len'
            | _::cdr -> lengthrec' cdr (len' + 1)
        in  lengthrec' list 0

end


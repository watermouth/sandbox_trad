(* item *)
 type t = Empty | JPY | USD | EUR

 let to_int = function
   JPY -> 0 
 | USD -> 1 
 | EUR -> 2 
 | Empty  -> -1

 let make = function
   0 -> JPY 
 | 1 -> USD 
 | 2 -> EUR
 | _ -> raise Not_found

 let to_string = function
   JPY -> "JPY"
 | USD -> "USD"
 | EUR -> "EUR"
 | Empty  -> "Empty"

(* test *)
let sample1 = JPY
let sample2 = USD
let sample3 = Empty

let test1 = make 0 = JPY
let test2 = make 1 = USD
let test11 = to_string sample1 = "JPY"

let tests = [test11;test2;test1]


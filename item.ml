(* item *)
 type t = USDJPY | EURJPY | EURUSD

 let to_int = function
   USDJPY -> 1 
 | EURJPY -> 2
 | EURUSD -> 3

 let make = function
   1 -> USDJPY
 | 2 -> EURJPY 
 | 3 -> EURUSD
 | _ -> raise Not_found

 let to_string = function
   USDJPY -> "USDJPY"
 | EURJPY -> "EURJPY"
 | EURUSD -> "EURUSD"

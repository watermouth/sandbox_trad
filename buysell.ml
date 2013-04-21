(* buysell *)
 type t = Buy | Sell

 let to_bs_type = function
 | 0 -> Buy
 | 1 -> Sell
 | _ -> raise Not_found

 let to_bs_code = function
 | Buy  -> 0
 | Sell -> 1

 let to_lot_sign = function
 | Buy  -> 1
 | Sell -> -1

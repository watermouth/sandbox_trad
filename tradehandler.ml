(* trade handler *)

(* set bid ask price of Price.t *)
let set_baprice trd ~price =
  let price = if trd.Trade.lot1_ >= 0.0 then price.Price.ask_ else price.Price.bid_ in
  Trade.set_price trd price




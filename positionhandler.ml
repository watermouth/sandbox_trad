(* position handler *)
(* This depends not only Position module but also others. *)

(* unit pl for open position *)
let calc_unit_pl pos price = match pos.Position.vwap_ with
  | Some vwap -> 
    if pos.Position.lot1_ > 0.0 then price.Price.bid_ -. vwap else vwap -. price.Price.ask_
  | None -> ( assert (pos.Position.lot1_ = 0.0); 0.0) (* lot1 is zero *)

(* profit loss calculation *)
(* ��Ϳ��position �� ɾ�������Υ졼�Ȥ��� PL �򻻽Ф��� *)
let calc_pl pos price =
  let lot = pos.Position.lot1_ in
  match pos.Position.vwap_ with
  | Some vwap -> 
    let (price, unit_pl) = if lot >= 0.0 
                           then (price.Price.bid_, price.Price.bid_ -. vwap)
                           else (price.Price.ask_, vwap -. price.Price.ask_) in 
    let latent = (lot *. price) in
    (unit_pl, unit_pl *. pos.Position.lot1_, pos.Position.lot2_ +. latent)
  | None -> (assert (lot = 0.0); (0.0, 0.0, pos.Position.lot2_))

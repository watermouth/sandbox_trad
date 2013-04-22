(* position *)
(* counter party 毎に持つべき? 少なくとも全通貨についてまとめて持つことは考えなければならない *)
open CalendarLib
(* abbreviation *)
module T = Trade 
 
type t = {
  seq_:		int;
  date_:	Date.t;
  time_:	Time.t;
  item_:	Item.t;
  lot_:		int;
  vwap_:	float;
  amt_:		float;
  }

let init () = {seq_=(~-1); date_=(Date.make 1900 1 1); time_=(Time.make 0 0 0);
               item_=Item.Empty; lot_=0; vwap_=0.0; amt_=0.0}

let calc_vwap pos trd = 
  (* 平均売買価格がいくらになるか. positionの符号が変わる場合, その取引の価格となる *) 
  (* positionの符号が変わらない場合, 加算対象となる取引のlotの符号とposの符号が同一なら, 加重平均,
     そうでなければposの価格のまま *)
  let sign_trd = (Buysell.to_lot_sign trd.T.bs_) in
  let signed_trd_lot = sign_trd * trd.T.lot_ in
  let signed_trd_amt = (float_of_int sign_trd) *. trd.T.amt_ in 
  let total_lot = pos.lot_ + signed_trd_lot in 
  if total_lot = 0 then 0.0
  (* 符号が逆転する場合 *)
  else if pos.lot_ >= 0 && total_lot < 0 || pos.lot_ <= 0 && total_lot > 0
    then match (trd.T.price_) with 
    | Some p -> p
    | None -> raise Not_found 
  (* 符号は変わらないが, 逆向きの取引を加える場合 *) 
  else if pos.lot_ > 0 && sign_trd < 0 || pos.lot_ < 0 && sign_trd > 0 
    then pos.vwap_
  (* 同一の向きの取引を加える場合 *)
  else (pos.amt_ +. signed_trd_amt ) /. (float_of_int total_lot)

let add pos trd = 
  match trd with
  | {T.seq_=seq; T.date_=date; T.time_=time; T.cpty_=cpty; T.item_=item; T.bs_=bs; T.lot_=lot;
     T.price_=price; T.amt_=amt} ->
    let sign_bs = Buysell.to_lot_sign bs in 
    {seq_=pos.seq_ + 1; date_=pos.date_; time_=pos.time_; item_=pos.item_;
     lot_=(pos.lot_ + sign_bs * lot);
     vwap_= (calc_vwap pos trd);
     amt_ = (pos.amt_ +. amt)} 
    
let to_string = function
  {seq_=seq; date_=date; time_=time; item_=item; lot_=lot; vwap_=vwap; amt_=amt} ->
  Printf.printf "%d, %d-%d-%d, %d%d%d, %s, %d, %3.5f, %.2f\n" 
    seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date) 
    (Time.hour time) (Time.minute time) (Time.second time)  
    (Item.to_string item) lot vwap amt

(* sample *)
let date1 = Date.make 2013 4 23;;
let time1 = Time.make 8 30 30;;
let sample1 = init ();;
to_string sample1;;



(* position *)
(* counter party 毎に持つべき? 少なくとも全通貨についてまとめて持つことは考えなければならない *)
open CalendarLib
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

let add pos trd = 
  let module T = Trade in
  match trd with
  | {T.seq_=seq; T.date_=date; T.time_=time; T.cpty_=cpty; T.item_=item; T.bs_=bs; T.lot_=lot;
     T.price_=price; T.amt_=amt} ->
    let sign_bs = Buysell.to_lot_sign bs in 
    let total_lot = pos.lot_ + (sign_bs * lot) in
(* vwap 計算は別に切り出すべき *)
(*
    let vwap =  
      if pos.lot_ > 0 then
        if total_lot > 0 then
          (pos.vwap_ *. (float_of_int pos.lot_) +. amt) /. (float_of_int total_lot)
        else if total_lot < 0 then
          price
        else 0.
      else if pos.lot < 0 then
        if total_lot < 0 then
          (pos.vwap_ *. (float_of_int pos.lot_) -. amt) /. (float_of_int total_lot)
*) 
      
    {seq_=pos.seq_ + 1; date_=pos.date_; time_=pos.time_; item_=pos.item_;
     lot_=(pos.lot_ + sign_bs * lot);
     vwap_=
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



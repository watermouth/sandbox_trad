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

let to_string = function
  {seq_=seq; date_=date; time_=time; item_=item; lot_=lot; vwap_=vwap; amt_=amt} ->
  Printf.printf "%d, %d-%d-%d, %d%d%d, %s, %d, %3.5f, %.2f" 
    seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date) 
    (Time.hour time) (Time.minute time) (Time.second time)  
    (Item.to_string item) lot vwap amt




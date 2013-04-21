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





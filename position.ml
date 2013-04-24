(* position *)
(* counter party 毎に持つべき? 少なくとも全通貨についてまとめて持つことは考えなければならない *)
(* しかしvwapはどうすればいい. vwapを求めるには特定にitem1, item2のペアについて考えなければならないのでは？*)

open CalendarLib
(* abbreviation *)
module T = Trade 
 
type t = {
  mutable seq_:		int ;
  mutable date_:	Date.t ;
  mutable time_:	Time.t ;
  item1_:		Item.t ;
  mutable lot1_:	float ;
  item2_:		Item.t ;
  mutable lot2_:	float ;
  mutable vwap_:	float option ;
  }

let init item1 item2 =
  {seq_=0; date_=(Date.make 1900 1 1); time_=(Time.make 0 0 0);
   item1_=item1; lot1_=0.;
   item2_=item2; lot2_=0.;
   vwap_= None
  }

let calc_vwap pos trd = 
  (* 平均売買価格がいくらになるか. positionの符号が変わる場合, その取引の価格となる *) 
  (* positionの符号が変わらない場合, 加算対象となる取引のlotの符号とposの符号が同一なら, 加重平均,
     そうでなければposの価格のまま *)
  let signed_trd_lot = trd.T.lot1_ in 
  let total_lot = pos.lot1_ +. signed_trd_lot in 
  if total_lot = 0.0 then None
  (* 符号が逆転する場合 *)
  else if pos.lot1_ >= 0.0 && total_lot < 0.0 || pos.lot1_ <= 0.0 && total_lot > 0.0
    then trd.T.price_
  (* 符号は変わらないが, 逆向きの取引を加える場合 *) 
  else if pos.lot1_ > 0.0 && trd.T.lot1_ < 0.0 || pos.lot1_ < 0.0 && trd.T.lot1_ > 0.0 
    then pos.vwap_ 
  (* 同一の向きの取引を加える場合 *)
  else match trd.T.lot2_ with
       | Some amt -> Some ((pos.lot2_ +. amt) /. (total_lot))
       | None -> None

let add pos trd = 
  match trd with
  | {T.seq_=seq; T.date_=date; T.time_=time;
     T.item1_=item1; T.lot1_=lot1;
     T.item2_=item2; T.lot2_=lot2;
     T.price_=price} -> 
    {seq_=pos.seq_ + 1; date_=date; time_=(match time with Some x -> x | None -> assert false);
     item1_=pos.item1_;
     item2_=pos.item2_;
     lot1_=(pos.lot1_ +. lot1);
     lot2_=(pos.lot2_ +. (match lot2 with Some l -> l | None -> assert false));
     vwap_= (calc_vwap pos trd);
    } 

let to_string = function
  {seq_=seq; date_=date; time_=time; 
   item1_=item1; lot1_=lot1;
   item2_=item2; lot2_=lot2; vwap_=vwap} -> 
  (Printf.printf "%6d, %d-%d-%d, %d:%d:%d, %s, %12.2f, %s, %12.2f, " 
    seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date) 
    (Time.hour time) (Time.minute time) (Time.second time)  
    (Item.to_string item1) lot1
    (Item.to_string item2) lot2);
  (match vwap with 
   | Some x -> Printf.printf "%3.6f\n" x
   | None -> print_newline ()
  )

(* sample *)
let date1 = Date.make 2013 4 23;;
let time1 = Time.make 8 30 30;;
let sample1 = init Item.USD Item.JPY;;
to_string sample1;;
let trd1 = (Trade.make 1 date1 (Some time1) "dummy" 1 12000. 0 (Some 98.82));;
let sample2 = add sample1 trd1;;
to_string sample2;;



(* position *)
(* counter party ��˻��Ĥ٤�? ���ʤ��Ȥ����̲ߤˤĤ��ƤޤȤ�ƻ��Ĥ��ȤϹͤ��ʤ���Фʤ�ʤ� *)
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
  (* ʿ��������ʤ�������ˤʤ뤫. position����椬�Ѥ����, ���μ���β��ʤȤʤ� *) 
  (* position����椬�Ѥ��ʤ����, �û��оݤȤʤ�����lot������pos����椬Ʊ��ʤ�, �ý�ʿ��,
     �����Ǥʤ����pos�β��ʤΤޤ� *)
  let sign_trd = (Buysell.to_lot_sign trd.T.bs_) in
  let signed_trd_lot = sign_trd * trd.T.lot_ in
  let signed_trd_amt = (float_of_int sign_trd) *. trd.T.amt_ in 
  let total_lot = pos.lot_ + signed_trd_lot in 
  if total_lot = 0 then 0.0
  (* ��椬��ž������ *)
  else if pos.lot_ >= 0 && total_lot < 0 || pos.lot_ <= 0 && total_lot > 0
    then match (trd.T.price_) with 
    | Some p -> p
    | None -> raise Not_found 
  (* �����Ѥ��ʤ���, �ո����μ����ä����� *) 
  else if pos.lot_ > 0 && sign_trd < 0 || pos.lot_ < 0 && sign_trd > 0 
    then pos.vwap_
  (* Ʊ��θ����μ����ä����� *)
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



(* position *)
(* counter party ��˻��Ĥ٤�? ���ʤ��Ȥ����̲ߤˤĤ��ƤޤȤ�ƻ��Ĥ��ȤϹͤ��ʤ���Фʤ�ʤ� *)
(* ������vwap�Ϥɤ�����Ф���. vwap�����ˤ������item1, item2�Υڥ��ˤĤ��ƹͤ��ʤ���Фʤ�ʤ��ΤǤϡ�*)

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
  (* ʿ��������ʤ�������ˤʤ뤫. position����椬�Ѥ����, ���μ���β��ʤȤʤ� *) 
  (* position����椬�Ѥ��ʤ����, �û��оݤȤʤ�����lot������pos����椬Ʊ��ʤ�, �ý�ʿ��,
     �����Ǥʤ����pos�β��ʤΤޤ� *)
  let signed_trd_lot = trd.T.lot1_ in 
  let total_lot = pos.lot1_ +. signed_trd_lot in 
  if total_lot = 0.0 then None
  (* ��椬��ž������ *)
  else if pos.lot1_ >= 0.0 && total_lot < 0.0 || pos.lot1_ <= 0.0 && total_lot > 0.0
    then trd.T.price_
  (* �����Ѥ��ʤ���, �ո����μ����ä����� *) 
  else if pos.lot1_ > 0.0 && trd.T.lot1_ < 0.0 || pos.lot1_ < 0.0 && trd.T.lot1_ > 0.0 
    then pos.vwap_ 
  (* Ʊ��θ����μ����ä����� *)
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



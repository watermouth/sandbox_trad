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
  let total_lot = pos.lot1_ +. trd.T.lot1_ in 
  if total_lot = 0.0 then None
  (* ��椬��ž���� �ޤ��� pos.vwap_��None�ξ��*)
  else if pos.lot1_ >= 0.0 && total_lot < 0.0 || pos.lot1_ <= 0.0 && total_lot > 0.0 || pos.vwap_ = None
    then trd.T.price_
  (* �����Ѥ��ʤ���, �ո����μ����ä����� *) 
  else if pos.lot1_ > 0.0 && trd.T.lot1_ < 0.0 || pos.lot1_ < 0.0 && trd.T.lot1_ > 0.0 
    then pos.vwap_ 
  (* Ʊ��θ����μ����ä����� *)
  else match (pos.vwap_, trd.T.lot2_) with
       (* pos.lot2_��ȤäƤϤ����ʤ�. vwap����׻�����. *) 
       | (Some vp, Some amt) -> Some ((pos.lot1_ *. vp -. amt) /. (total_lot)) (* lot2's sign is minus of lot1_*)
       | (_,_) -> None

let copy pos =
    {seq_=pos.seq_; date_=pos.date_; time_=pos.time_;
     item1_=pos.item1_; item2_=pos.item2_; lot1_=pos.lot1_; lot2_=pos.lot2_; vwap_=pos.vwap_
    } 

let add ?mode:(do_override = true) pos trd = 
  match trd with
  | {T.seq_=seq; T.date_=date; T.time_=time;
     T.item1_=item1; T.lot1_=lot1;
     T.item2_=item2; T.lot2_=lot2;
     T.price_=price} -> 
    let pos = if do_override then pos else copy pos in
      let vwap = calc_vwap pos trd in
      (pos.seq_ <- pos.seq_ + 1;
      pos.date_ <- date;
      pos.time_ <- (match time with Some x -> x | None -> assert false);
      pos.lot1_ <- pos.lot1_ +. lot1;
      pos.lot2_ <- pos.lot2_ +. (match lot2 with Some l -> l | None -> assert false);
      pos.vwap_ <- vwap;
      pos)

let add_trades ?mode:(do_override = true) pos trd_array =
  let p = ref pos in
  (Array.iter (fun trd -> p := add ~mode:do_override !p trd) trd_array;
   !p)

let add_trade_list pos trd_list =
  let rec sub p lst = match lst with
  | [] -> p
  | h::t -> sub (add p h) t 
  in sub pos trd_list

(* profit loss calculation *)
(* ��Ϳ��position �� ɾ�������Υ졼�Ȥ��� PL �򻻽Ф��� *)
let calc_pl pos price =
  let latent = (pos.lot1_ *. price) in
  (pos.lot2_, latent, pos.lot2_ +. latent)

let to_string = function
  {seq_=seq; date_=date; time_=time; 
   item1_=item1; lot1_=lot1;
   item2_=item2; lot2_=lot2; vwap_=vwap} -> 
  (Printf.printf "%6d, %4d-%02d-%02d, %02d:%02d:%02d, %s, %12.2f, %s, %12.2f, " 
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
let trd1 = (Trade.make 1 date1 (Some time1) "dummy" 1 12000. 0 (Some 98.82));;
let trd2 = (Trade.make 1 date1 (Some time1) "dummy" 1 ~-.12000. 0 (Some 98.80));;
let trd3 = (Trade.make 1 date1 (Some time1) "dummy" 1 ~-.140000. 0 (Some 98.78));;
let trd4 = (Trade.make 1 date1 (Some time1) "dummy" 1 140000. 0 (Some 98.78));;
let trd10 = (Trade.make 1 date1 (Some time1) "dummy" 1 12000. 0 (Some 98.82));;
let trd11 = (Trade.make 1 date1 (Some time1) "dummy" 1 5000. 0 (Some 98.80));;
let trd_array1 = [|trd10; trd11|];;
let sample1 = init Item.USD Item.JPY;;
let sample2 = add ~mode:false sample1 trd1;;
let sample3 = 
   {seq_=1;date_=date1;time_=time1;
    item1_=Item.USD;lot1_=100000.0;item2_=Item.JPY;lot2_=(~-.9900000.0);
    vwap_=None
   };;
let sample4 = 
   {seq_=1;date_=date1;time_=time1;
    item1_=Item.USD;lot1_=100000.0;item2_=Item.JPY;lot2_=(~-.9900000.0);
    vwap_=Some 99.01
   };;
let sample5 = 
   {seq_=1;date_=date1;time_=time1;
    item1_=Item.USD;lot1_=(~-.100000.0);item2_=Item.JPY;lot2_=(9900000.0);
    vwap_=None
   };;
let sample6 = 
   {seq_=1;date_=date1;time_=time1;
    item1_=Item.USD;lot1_=(~-.100000.0);item2_=Item.JPY;lot2_=(9900000.0);
    vwap_=Some 99.01
   };;
to_string sample1;;
to_string sample2;;

(* test *)
let test_result = ref [];;
(* test calc_vwap *)
(* Position lot = 0: �ݥ������̵���˲û� *)
let test_calc_vwap1 = (calc_vwap sample1 trd1) = Some 98.82
let test_calc_vwap2 = (calc_vwap sample1 trd2) = Some 98.80

(* Position lot > 0 : �ݥ�������� *)
(* vwap None �˲û� *) 
let test_calc_vwap3 = (calc_vwap sample3 trd1) = Some 98.82
let test_calc_vwap4 = (calc_vwap sample3 trd2) = Some 98.80
let test_calc_vwap5 = (calc_vwap sample3 trd3) = Some 98.78
(* vwap Some x �˲û� *)
let test_calc_vwap6 = (calc_vwap sample4 trd1) = (*Ʊ�������*) 
  let lot2 = match trd1.T.lot2_ with Some x -> x | None -> 0.0 in
  let vp = match sample4.vwap_ with Some x -> x | None -> assert false in 
  Some ((sample4.lot1_ *. vp -. lot2) /. (sample4.lot1_ +. trd1.T.lot1_))
let test_calc_vwap7 = (calc_vwap sample4 trd2) = (*�������: pos������� *)
  sample4.vwap_
let test_calc_vwap8 = (calc_vwap sample4 trd3) = (*�������: pos����Ѳ� *)
  trd3.T.price_

(* Postion lot < 0 *)
(* vwap None �˲û� *) 
let test_calc_vwap103 = (calc_vwap sample5 trd2) = Some 98.80
let test_calc_vwap104 = (calc_vwap sample5 trd1) = Some 98.82
let test_calc_vwap105 = (calc_vwap sample5 trd4) = Some 98.78 (* not trd3 *)
(* vwap Some x �˲û� *)
let test_calc_vwap106 = (calc_vwap sample6 trd2) = (*Ʊ�������*) 
  let lot2 = match trd2.T.lot2_ with Some x -> x | None -> 0.0 in
  let vp = match sample6.vwap_ with Some x -> x | None -> assert false in 
  Some ((sample6.lot1_ *. vp -. lot2) /. (sample6.lot1_ +. trd2.T.lot1_))
let test_calc_vwap107 = (calc_vwap sample6 trd1) = (*�������: pos������� *)
  sample6.vwap_
let test_calc_vwap108 = (calc_vwap sample6 trd4) = (*�������: pos����Ѳ� *)
  trd4.T.price_

(* test: add_trades *)
let test_add_trades1 = 
  let p = ref (init Item.USD Item.JPY) in
  (p := add_trades ~mode:true !p trd_array1;
  (!p).lot1_)
  = 17000.0
let test_add_trades2 = 
  let p = ref (init Item.USD Item.JPY) in
  (p := add_trades ~mode:false !p trd_array1;
  (!p).lot1_)
  = 17000.0

(* test: add_trade_list *)
let test_add_trade_list1 =
  let p = ref (init Item.USD Item.JPY) in
  (p := add_trade_list !p (Array.to_list trd_array1);
  (!p).lot1_)
  = 17000.0
let test_add_trade_list2 = 
  let p = ref (init Item.USD Item.JPY) in
  (p := add_trade_list !p (Array.to_list trd_array1);
  (!p).lot1_)
  = 17000.0


(* test: calc_pl *)
let trd101 = (Trade.make 1 date1 (Some time1) "dummy" 1 1000. 0 (Some 98.82));;
let trd102 = (Trade.make 1 date1 (Some time1) "dummy" 1 ~-.1000. 0 (Some 98.80));;
let test_calc_pl1 = 
  let p = init Item.USD Item.JPY in
  calc_pl p 100.0 = (0.0, 0.0, 0.0)
let test_calc_pl2 =
  let p = ref (init Item.USD Item.JPY) in
  (p:= (add ~mode:false !p trd101);
  calc_pl !p 100.0 = (~-. 98820.0, 100000.0, 1180.0)) 
let test_calc_pl3 =
  let p = ref (init Item.USD Item.JPY) in
  (p:= (add ~mode:false !p trd102);
  calc_pl !p 100.0 = (98800.0, ~-. 100000.0, ~-. 1200.0));;

test_result := test_calc_vwap1 :: test_calc_vwap2 :: test_calc_vwap3 :: test_calc_vwap4 ::
  test_calc_vwap5 :: test_calc_vwap6 :: test_calc_vwap7 :: test_calc_vwap8 :: test_calc_vwap103 :: 
  test_calc_vwap104 :: test_calc_vwap105 :: test_calc_vwap106 :: test_calc_vwap107 :: test_calc_vwap108 :: 
  test_add_trades1 :: test_add_trades2 :: test_add_trades1 :: test_add_trades2 :: 
  test_calc_pl1 :: test_calc_pl2 :: test_calc_pl3 :: !test_result;;

Printf.printf "Position test result: %b\n" (List.for_all (fun x -> x) !test_result)

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
  let total_lot = pos.lot1_ +. trd.T.lot1_ in 
  if total_lot = 0.0 then None
  (* 符号が逆転する または pos.vwap_がNoneの場合*)
  else if pos.lot1_ >= 0.0 && total_lot < 0.0 || pos.lot1_ <= 0.0 && total_lot > 0.0 || pos.vwap_ = None
    then trd.T.price_
  (* 符号は変わらないが, 逆向きの取引を加える場合 *) 
  else if pos.lot1_ > 0.0 && trd.T.lot1_ < 0.0 || pos.lot1_ < 0.0 && trd.T.lot1_ > 0.0 
    then pos.vwap_ 
  (* 同一の向きの取引を加える場合 *)
  else match (pos.vwap_, trd.T.lot2_) with
       (* pos.lot2_を使ってはいけない. vwapから計算する. *) 
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
(* 所与のposition と 評価時点のレートから PL を算出する *)
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
(* Position lot = 0: ポジション無しに加算 *)
let test_calc_vwap1 = (calc_vwap sample1 trd1) = Some 98.82
let test_calc_vwap2 = (calc_vwap sample1 trd2) = Some 98.80

(* Position lot > 0 : ポジションが正 *)
(* vwap None に加算 *) 
let test_calc_vwap3 = (calc_vwap sample3 trd1) = Some 98.82
let test_calc_vwap4 = (calc_vwap sample3 trd2) = Some 98.80
let test_calc_vwap5 = (calc_vwap sample3 trd3) = Some 98.78
(* vwap Some x に加算 *)
let test_calc_vwap6 = (calc_vwap sample4 trd1) = (*同一符号取引*) 
  let lot2 = match trd1.T.lot2_ with Some x -> x | None -> 0.0 in
  let vp = match sample4.vwap_ with Some x -> x | None -> assert false in 
  Some ((sample4.lot1_ *. vp -. lot2) /. (sample4.lot1_ +. trd1.T.lot1_))
let test_calc_vwap7 = (calc_vwap sample4 trd2) = (*異符号取引: pos符号不変 *)
  sample4.vwap_
let test_calc_vwap8 = (calc_vwap sample4 trd3) = (*異符号取引: pos符号変化 *)
  trd3.T.price_

(* Postion lot < 0 *)
(* vwap None に加算 *) 
let test_calc_vwap103 = (calc_vwap sample5 trd2) = Some 98.80
let test_calc_vwap104 = (calc_vwap sample5 trd1) = Some 98.82
let test_calc_vwap105 = (calc_vwap sample5 trd4) = Some 98.78 (* not trd3 *)
(* vwap Some x に加算 *)
let test_calc_vwap106 = (calc_vwap sample6 trd2) = (*同一符号取引*) 
  let lot2 = match trd2.T.lot2_ with Some x -> x | None -> 0.0 in
  let vp = match sample6.vwap_ with Some x -> x | None -> assert false in 
  Some ((sample6.lot1_ *. vp -. lot2) /. (sample6.lot1_ +. trd2.T.lot1_))
let test_calc_vwap107 = (calc_vwap sample6 trd1) = (*異符号取引: pos符号不変 *)
  sample6.vwap_
let test_calc_vwap108 = (calc_vwap sample6 trd4) = (*異符号取引: pos符号変化 *)
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

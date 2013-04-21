(* Trade module *)
(* #require "calendar" *)
 (* trade: sequence number, date, time, item code(ccyPair id), buy-sell code, lot, price *)
 (*                    int, Date, Time, int,  *)
open CalendarLib
 type t = {
   seq_:	int;
   date_:	CalendarLib.Date.t;
   time_:	CalendarLib.Time.t option;
   cpty_:	Counterparty.t;
   item_:	Item.t;
   bs_:		Buysell.t;
   lot_:	int;
   price_:	float option;
   amt_:	float;
 }

 let make seq date time cpty item bs lot price =
   {seq_=seq; date_=date; time_=time; cpty_=(Counterparty.make cpty); item_=(Item.make item);
    bs_=(Buysell.to_bs_type bs); lot_=lot; price_=price;
    amt_= match price with 
      | Some x -> ((float_of_int lot) *. x)
      | None -> 0.0
   }

 let to_string {seq_=seq; date_=date; time_=time; cpty_=cpty; item_=item; bs_=bs; lot_=lot; price_=price} =  
   match (time, price) with 
   | (Some vTime, Some vPrice) -> let time = vTime in let price = vPrice in
     Printf.printf "seq:%06d, date:%d-%02d-%02d, time:%d:%02d:%02d, cpty:%s, item:%s, bs:%d, lot:%d, price:%3.5f\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       (Time.hour time) (Time.minute time) (Time.second time) 
       (Counterparty.to_string cpty)
       (Item.to_string item) (Buysell.to_bs_code bs) lot price
   | (_, _) -> 
     Printf.printf "seq:%06d, date:%d-%02d-%02d, time:Undefined, cpty:%s, item:%s, bs:%d, lot:%d, price:Undefined\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       (Counterparty.to_string cpty)
       (Item.to_string item) (Buysell.to_bs_code bs) lot 

(* test *)
;;
CalendarLib.Time_Zone.change CalendarLib.Time_Zone.Local
let date1 = CalendarLib.Date.make 2013 4 22 
let time1 = CalendarLib.Time.make 22 0 1
let sample1 = make 1 date1 (Some time1) "dummy" 1 0 10000 (Some 94.325)

let sample =
  Random.init 8888;
  let num = 10 in 
  Array.append
  (Array.init num
    (fun i -> make i date1 (Some (Time.add (Time.make 7 0 0) (Time.Period.second i)))
              "dummy" 1 (Random.int 2) 10000 (Some 89.321))
  )
  (Array.init num
    (fun i -> make (i+num) date1 None "dummy" 1 (Random.int 2) 10000 None) 
  );;

(* show *)
Array.iter (fun x -> to_string x) sample


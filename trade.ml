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
   item1_:	Item.t;
   lot1_:	float;
   item2_:	Item.t;
   lot2_:	float option;
   price_:	float option;
 }

 let make seq date time cpty item1 lot1 item2 price =
   {seq_=seq; date_=date; time_=time; cpty_=(Counterparty.make cpty);
    item1_=(Item.make item1); item2_=(Item.make item2);
    lot1_ =lot1;
    lot2_ = (match price with 
      | Some x -> Some (x *. lot1) 
      | None -> None);
    price_=price
   }

 let to_string {seq_=seq; date_=date; time_=time; cpty_=cpty;
                item1_=item1; lot1_=lot1; item2_=item2; lot2_=lot2;price_=price} = 
   match (time, price, lot2) with 
   | (Some vTime, Some vPrice, Some vLot2) -> let time = vTime in let price = vPrice in let lot2 = vLot2 in
     Printf.printf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%s,%12.2f,%s,%12.2f,%3.5f\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       (Time.hour time) (Time.minute time) (Time.second time) 
       (Counterparty.to_string cpty)
       (Item.to_string item1) lot1 
       (Item.to_string item2) lot2 
       price
   | (_, _, _) -> 
     Printf.printf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%s,%12.2f,%s,,\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       0 0 0 
       (Counterparty.to_string cpty)
       (Item.to_string item1) lot1 
       (Item.to_string item2)
  let header = "seq,date,time,item1,lot1,item2,lot2,price"

(* test *)
;;
CalendarLib.Time_Zone.change CalendarLib.Time_Zone.Local
let date1 = CalendarLib.Date.make 2013 4 22 
let time1 = CalendarLib.Time.make 22 0 1
let sample1 = make 1 date1 (Some time1) "dummy" 1 10000. 0 (Some 94.325)

let sample =
  Random.init 8888;
  let num = 10 in 
  Array.append
  (Array.init num
    (fun i -> make i date1 (Some (Time.add (Time.make 7 0 0) (Time.Period.second i)))
              "dummy" 1 (10000. *. (if Random.bool () then 1. else (-1.))) 0 (Some 89.321))
  )
  (Array.init num
    (fun i -> make (i+num) date1 None "dummy" 2 10000. 0 None) 
  );;

(* show *)
Array.iter (fun x -> to_string x) sample


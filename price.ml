(* price module *)
open CalendarLib
type t = {
  seq_:	int;
  date_:    CalendarLib.Date.t;
  time_:    CalendarLib.Time.t;
  item_:    Item.t;
  bid_:	float;
  mid_:     float;
  ask_:     float;
}

let bid {bid_=b} = b
let mid {mid_=m} = m
let ask {ask_=a} = a

let make seq date time item bid mid ask =
  {seq_=seq; date_=date; time_=time; item_=(Item.make item); bid_=bid; mid_=mid; ask_=ask}

let header = "seq, date, time, item, bid, mid, ask\n"

let to_string = function
| {seq_=seq; date_=date; time_=time; item_=item; bid_=bid; mid_=mid; ask_=ask} ->
  Printf.printf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%3.5f,%3.5f,%3.5f\n"
    seq 
    (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
    (Time.hour time) (Time.minute time) (Time.second time) 
    (Item.to_string item) bid mid ask

let from_array_to_string a = Array.iter (fun x -> to_string x) a

(* test *)
;;
CalendarLib.Time_Zone.change CalendarLib.Time_Zone.Local
let date1 = CalendarLib.Date.make 2013 4 22 
let time1 = CalendarLib.Time.make 7 0 0
let sample_bid = 97.824
let sample_ask = 97.834
let sample_mid = (sample_bid +. sample_ask) /. 2.0
let sample1 = make 0 date1 time1 1(*USDJPY*) sample_bid sample_mid sample_ask
;;
to_string sample1;;

let make_samples num =
  Random.init 8888;
  Array.init num 
  (fun i -> let diff = Random.float 0.001 in
    let b = sample_bid +. if Random.bool () then diff else (~-.diff) in
    let a = sample_ask +. if Random.bool () then diff else (~-.diff) in
    make i date1 (Time.add time1 (Time.Period.second i)) 2 
    b ((b +. a) /. 2.) a
  );;

from_array_to_string (make_samples 10)


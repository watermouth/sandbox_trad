(* price module *)
open CalendarLib
type t = {
  seq_:	int;
  date_:    CalendarLib.Date.t;
  time_:    CalendarLib.Time.t;
  item1_:    Item.t;
  item2_:    Item.t;
  bid_:	float;
  mid_:     float;
  ask_:     float;
}

let make seq date time item1 item2 bid mid ask =
  {seq_=seq; date_=date; time_=time; item1_=(Item.make item1);
   item2_=(Item.make item2);  bid_=bid; mid_=mid; ask_=ask}

let to_string ?(crlf=true) 
  {seq_=seq; date_=date; time_=time; item1_=item1;item2_=item2; bid_=bid; mid_=mid; ask_=ask} = 
  let s = Printf.sprintf "%6d,%d-%02d-%02d,%d:%02d:%02d,%d,%d,%3.5f,%3.5f,%3.5f"
    seq 
    (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
    (Time.hour time) (Time.minute time) (Time.second time) 
    (Item.to_int item1) (Item.to_int item2)
    bid mid ask
  in 
  if crlf then s ^ "\n" else s 

let header = "seq, date, time, item1, item2, bid, mid, ask"

let of_string_array s (* data as string array *) = 
  make (int_of_string s.(0)) (Datetimehelper.date_of_string s.(1)) (Datetimehelper.time_of_string s.(2))
       (int_of_string s.(3)) (int_of_string s.(4)) 
       (float_of_string s.(5)) (float_of_string s.(6)) (float_of_string s.(7)) 

let from_array_to_string a = 
  let s = Array.fold_left (fun x y -> x ^ (to_string y) ) "" a in
  (s)

let to_csv ~name a =
  let oo = open_out name in
  Array.iter (fun x -> output_string oo (to_string x)) a;
  close_out oo;
  ()

let load_from_csv filename =
  let dc = Csv.load filename in
  let da = Csv.to_array dc in
  Array.map (fun x -> of_string_array x) da
  
  (* let a_header = Str.split (Str.regexp_string ",") header in *) (* botu *)
  (* let Csv.associate a_header dc *)

(* test *)
;;
CalendarLib.Time_Zone.change CalendarLib.Time_Zone.Local
let date1 = CalendarLib.Date.make 2013 4 22 
let time1 = CalendarLib.Time.make 7 0 0
let sample_bid = 97.824
let sample_ask = 97.834
let sample_mid = (sample_bid +. sample_ask) /. 2.0
let sample1 = make 0 date1 time1 1 0 (*USDJPY*) sample_bid sample_mid sample_ask
;;
to_string sample1;;

let make_samples ?(item1_code=1) ?(item2_code=0) num =
  (* Random.init 8888;  seed cannot be set *)
  Array.init num 
  (fun i -> 
    let b = sample_bid +. Rmath.rnorm ~mean:0.0 ~sd:0.0001 () in
    let a = sample_ask +. Rmath.rnorm ~mean:0.0 ~sd:0.0001 () in
    make i date1 (Time.add time1 (Time.Period.second i)) item1_code item2_code
    b ((b +. a) /. 2.) a
  );;

let make_samples_old ?(item1_code=1) ?(item2_code=0) num =
  Random.init 8888;
  Array.init num 
  (fun i -> let diff = Random.float 0.001 in
    let b = sample_bid +. if Random.bool () then diff else (~-.diff) in
    let a = sample_ask +. if Random.bool () then diff else (~-.diff) in
    make i date1 (Time.add time1 (Time.Period.second i)) item1_code item2_code
    b ((b +. a) /. 2.) a
  );;

from_array_to_string (make_samples 10);;

Printf.printf "Price test result:nothing\n";;

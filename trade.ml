(* Trade module *)
 (* trade: sequence number, date, time, item code(ccyPair id), buy-sell code, lot, price *)
 (*                    int, Date, Time, int,  *)
open CalendarLib
type t = {
   seq_:	int;
   date_:	Date.t;
   time_:	Time.t option;
   cpty_:	Counterparty.t;
   item1_:	Item.t;
   lot1_:	float;
   item2_:	Item.t;
   mutable lot2_:	float option;
   mutable price_:	float option;
}

let make seq date time cpty item1 lot1 item2 price =
   {seq_=seq; date_=date; time_=time; cpty_=(Counterparty.make cpty);
    item1_=(Item.make item1); item2_=(Item.make item2);
    lot1_ =lot1;
    lot2_ = (match price with 
      | Some x -> Some (x *. ~-.lot1) 
      | None -> None);
    price_=price
 }

let set_price trd ~price = 
  trd.price_ <- Some price;
  trd.lot2_ <- Some (~-. price *. trd.lot1_);
  trd

let header = "seq,date,time,item1,lot1,item2,lot2,price"

let of_string_array s = 
  {seq_=(int_of_string s.(0));date_=(Iocommon.date_of_string s.(1));
   time_=Some (Iocommon.time_of_string (s.(2)));
   cpty_=(Counterparty.make s.(3));
   item1_=(Item.make (int_of_string s.(4))); lot1_=(float_of_string s.(5));
   item2_=(Item.make (int_of_string s.(6))); lot2_= Some (float_of_string s.(7));
   price_=Some (float_of_string s.(7))
  }

let to_string {seq_=seq; date_=date; time_=time; cpty_=cpty;
                item1_=item1; lot1_=lot1; item2_=item2; lot2_=lot2;price_=price} = 
   let s = match (time, price, lot2) with 
   | (Some vTime, Some vPrice, Some vLot2) -> let time = vTime in let price = vPrice in let lot2 = vLot2 in
     Printf.sprintf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%d,%12.2f,%d,%12.2f,%3.5f\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       (Time.hour time) (Time.minute time) (Time.second time) 
       (Counterparty.to_string cpty)
       (Item.to_int item1) lot1 
       (Item.to_int item2) lot2 
       price
   | (_, _, _) -> 
     Printf.sprintf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%d,%12.2f,%d,,\n"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       0 0 0 
       (Counterparty.to_string cpty)
       (Item.to_int item1) lot1 
       (Item.to_int item2)
   in (print_string s; s)

let load_from_csv fn =
  let dc = Csv.load fn in
  let da = Csv.to_array dc in
  Array.map (fun x -> of_string_array x) da

(* show *)
let from_array_to_string a = 
  let s = Array.fold_left (fun x y -> x ^ (to_string y)) "" a in
  (print_string s; s)


(* test *)
;;
Time_Zone.change Time_Zone.Local
let date1 = Date.make 2013 4 22 
let time1 = Time.make 22 0 1
let sample1 = make 1 date1 (Some time1) "dummy" 1 10000. 0 (Some 94.325)
let make_samples ?mode:(flg:bool=true) num =
  Random.init 8888;
  if (flg) then
    Array.init num
      (fun i -> make i date1 (Some (Time.add (Time.make 7 0 0) (Time.Period.second i)))
              "dummy" 1 (10000. *. (if Random.bool () then 1. else (-1.))) 0 (Some 89.321))
  else
    (Array.init num
    (fun i -> make (i+num) date1 None "dummy" 2 10000. 0 None) 
    );;

let test1 = sample1 = {seq_=1;date_=date1;time_=(Some time1);cpty_=(Counterparty.make "dummy");
                       item1_=(Item.make 1); lot1_=10000.0;
                       item2_=(Item.make 0); lot2_=(Some (~-. 94.325 *. 10000.0));
                       price_=(Some 94.325)}
let test_set_price1 =
  let trd = make 1 date1 (Some time1) "dummy2" 1 10000.0 0 None in
  let trd = set_price trd 100.0 in 
  trd = (make 1 date1 (Some time1) "dummy2" 1 10000.0 0 (Some 100.0))

  (* sample output exampl
print_string "sample output file name:"
output_file (read_line ()) (Trade.from_array_to_string (Trade.make_samples 10))
*)


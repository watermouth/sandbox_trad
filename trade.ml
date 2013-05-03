(* Trade module *)
 (* trade: sequence number, date, time, item code(ccyPair id), buy-sell code, lot, price *)
 (*                    int, Date, Time, int,  *)
open CalendarLib
type t = {
   seq_:	int;
   date_:	Date.t;
   time_:	Time.t option;
   cpty_:	Counterparty.t option;
   item1_:	Item.t;
   lot1_:	float;
   item2_:	Item.t;
   mutable lot2_:	float option;
   mutable price_:	float option;
}

let make seq date time cpty item1 lot1 item2 price =
   {seq_=seq; date_=date; time_=time; cpty_=(if cpty = "" then None else Some (Counterparty.make cpty));
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
  let lot1 = float_of_string s.(5) in
  let price = float_of_string s.(8) in 
  {seq_=(int_of_string s.(0));date_=(Datetimehelper.date_of_string s.(1));
   time_=Some (Datetimehelper.time_of_string (s.(2)));
   cpty_=(Some (Counterparty.make s.(3)));
   item1_=(Item.make (int_of_string s.(4))); lot1_=lot1;
   item2_=(Item.make (int_of_string s.(6))); lot2_= Some (price *. ~-.lot1);
   price_=Some price
  }

let to_string ?(crlf=true) {seq_=seq; date_=date; time_=time; cpty_=cpty;
                item1_=item1; lot1_=lot1; item2_=item2; lot2_=lot2;price_=price} = 
   let cpty = match cpty with Some c -> Counterparty.to_string c | None -> "" in 
   let (hh,mm,ss) = match time with
     | Some x -> ((Time.hour x), (Time.minute x), (Time.second x))
     | None -> (0,0,0)
   in
   let s = match (price, lot2) with 
   | (Some vPrice, Some vLot2) -> let price = vPrice in let lot2 = vLot2 in
     Printf.sprintf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%d,%12.2f,%d,%12.2f,%3.5f"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       hh mm ss 
       (cpty)
       (Item.to_int item1) lot1 
       (Item.to_int item2) lot2 
       price
   | (_, _) -> 
     Printf.sprintf "%6d,%d-%02d-%02d,%d:%02d:%02d,%s,%d,%12.2f,%d,,"
       seq (Date.year date) (Date.int_of_month (Date.month date)) (Date.day_of_month date)
       hh mm ss 
       (cpty)
       (Item.to_int item1) lot1 
       (Item.to_int item2)
   in 
   if crlf then s ^ "\n" else s 

let to_csv ~name a =
  let oc = open_out name in
  Array.iter (fun x -> output_string oc (to_string x)) a;
  close_out oc

let load_from_csv fn =
  let dc = Csv.load fn in
  let da = Csv.to_array dc in
  Array.map (fun x -> of_string_array x) da

(* show *)
let from_array_to_string a = 
  let s = Array.fold_left (fun x y -> x ^ (to_string y)) "" a in
  (s)


(* test *)
;;
Time_Zone.change Time_Zone.Local
let test_result = ref [];;
let date1 = Date.make 2013 4 22 
let time1 = Time.make 22 0 1
let sample1 = make 1 date1 (Some time1) "dummy" 1 10000. 0 (Some 94.325)
let make_samples ?mode:(flg:bool=true) ?(hpr=Array.create 1 Price.sample1) ?(interval=10.0) num =
  (* cannot set seed... Random.init 8888; *)
  (* sample times *)
  let seconds = Array.map
    (fun x -> x + (int_of_float (Rmath.rexp ~rate:interval ()))) (Array.create num 0) in
  for i=1 to (num-1) do
    seconds.(i) <- seconds.(i) + seconds.(i-1)
  done;
  (* Array.iter (fun x-> Printf.printf "%d\n" x) seconds; *)
  let times = let time_begin = hpr.(0).Price.time_ in 
    Batteries.Array.filter_map 
    (fun x -> if x <= 86400 then Some (Time.add time_begin (Time.Period.second x)) else None ) seconds in 
  (* reset num *) 
  let num = Array.length times in  
  (* set bid or ask and prices *)
  (* buy:1, sell:-1 *) 
  let signs = Array.init num (fun i -> if Random.bool () then 1 else ~-1) in 
  (* make hashtable *)
  let hpr_hash = Hashtbl.create num in
  (Array.iter (fun x -> Hashtbl.add hpr_hash x.Price.time_ x) hpr);
  let prices = Array.init num 
    (fun i -> match Hashtbl.find hpr_hash times.(i) with 
              | pr -> if signs.(i) > 0 then pr.Price.ask_ else pr.Price.bid_ ) in 
  if (flg) then
    Array.init num
      (fun i -> make i date1 (Some times.(i))
              "dummy" 1 ((float_of_int signs.(i)) *. 1000. *. (Rmath.rpois 10.)) 0
              (Some prices.(i))) 
  else
    (Array.init num
    (fun i -> make (i+num) date1 None "dummy" 1 10000. 0 None) 
    );;

let make_samples_01 ?mode:(flg:bool=true) num =
  Random.init 8888;
  if (flg) then
    Array.init num
      (fun i -> make i date1 (Some (Time.add (Time.make 7 0 0) (Time.Period.second i)))
              "dummy" 1 (10000. *. (if (Random.int 10) < 6 then 1. else (-1.))) 0 (Some 90.000))
  else
    (Array.init num
    (fun i -> make (i+num) date1 None "dummy" 1 10000. 0 None) 
    );;

let test1 = sample1 = {seq_=1;date_=date1;time_=(Some time1);cpty_=(Some (Counterparty.make "dummy"));
                       item1_=(Item.make 1); lot1_=10000.0;
                       item2_=(Item.make 0); lot2_=(Some (~-. 94.325 *. 10000.0));
                       price_=(Some 94.325)}
let test_set_price1 =
  let trd = make 1 date1 (Some time1) "dummy2" 1 10000.0 0 None in
  let trd = set_price trd 100.0 in 
  trd = (make 1 date1 (Some time1) "dummy2" 1 10000.0 0 (Some 100.0));;

test_result := test1 :: test_set_price1 :: !test_result;;
Printf.printf "Trade test result: %b\n" (List.for_all (fun x -> x) !test_result)

let time2 = Time.make 0 0 25200;;
let sample2 = make 1000 date1 (Some time2) "dummy2" 1 10000. 0 None;;


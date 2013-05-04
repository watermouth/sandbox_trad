(* sample data generation *)
(* sample prices *)
let num = 10;; 
Printf.printf "sample output file name(price)\n:";;
let cp_ofn = read_line ();;
let cp = (Price.make_samples num);;
output_file (cp_ofn) (Price.from_array_to_string cp) ;;
(* let cp_hash = Hashtbl.create num;;
Array.iter (fun x -> Hashtbl.add cp_hash x.Price.time_ x) cp;;
*)
print_string (Price.from_array_to_string cp);; 

(* sample trades *)
print_string "sample output file name:\n";;
let trd_ofn = read_line ();;
output_file (trd_ofn) (Trade.from_array_to_string (Trade.make_samples num ~hpr:cp ~interval:1.0 ));;

(* binary format data generation *)
Printf.printf "input data id(:positive integer) :";;
flush_all ();;
let fn_id = (read_line());;
let fn_tail = ".dat";;
let fn_trade = "tdtr";;
let fn_price = "tdpr";;
let fn_simulate = "tdsm";;
let fn_cover = "tdcv";;

(* price *)
let prices = Price.make_samples 86400;;
let ob = open_out_bin (fn_price ^ fn_id ^ fn_tail) in
  output_value ob prices;
  close_out ob;;

(* how to load *)
let price_loaded =
  let ib = open_in_bin (fn_price ^ fn_id ^ fn_tail) in
  let v : Price.t array = (input_value ib) in
  close_in ib; v

(* test *)
let test_save_and_load = Array.for_all2 (fun x y -> x = y) prices price_loaded;;

(* trade *)
let trades = Trade.make_samples 10000 ~hpr:prices ~interval:100.0 ~bias:0.8;;
let trades = Array.append trades (Trade.make_samples 100 ~hpr:prices ~interval:1000.0 ~lambda:1000.0);;
let ob = open_out_bin (fn_trade ^ fn_id ^ fn_tail);; 
output_value ob trades;;
close_out ob;;

(* how to load *)
let trade_loaded = 
  let ib = open_in_bin (fn_trade ^ fn_id ^ fn_tail) in
  let v : Trade.t array = (input_value ib) in
  close_in ib;
  v

(* test *)
let test_save_and_load = Array.for_all2 (fun x y -> x = y) trades trade_loaded;;

(* csv output *)
let fn_tail_csv = ".csv";;
Price.to_csv (fn_price ^ fn_id ^ fn_tail_csv) prices;;
Trade.to_csv (fn_trade ^ fn_id ^ fn_tail_csv) trades;;


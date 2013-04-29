(* sample data generation *)
(* sample trades *)
print_string "sample output file name:\n";;
let trd_ofn = read_line ();;
output_file (trd_ofn) (Trade.from_array_to_string (Trade.make_samples 10));;

(* sample prices *)
Printf.printf "sample output file name(price)\n:";;
let cp_ofn = read_line ();;
output_file (cp_ofn) (Price.from_array_to_string (Price.make_samples 10));;

(* binary format data generation *)
(* trade *)
let trades = Trade.make_samples 3000;;
let ob = open_out_bin "trade_testdata100.dat";;
output_value ob trades;;
close_out ob;;

(* how to load *)
let trade_loaded = 
  let ib = open_in_bin "trade_testdata100.dat" in
  let v : Trade.t array = (input_value ib) in
  v

(* test *)
let test_save_and_load = Array.for_all2 (fun x y -> x = y) trades trade_loaded;;

(* price *)
let prices = Price.make_samples 86400;;
let ob = open_out_bin "price_testdata100.dat" in
  output_value ob prices;
  close_out ob;;

(* how to load *)
let price_loaded =
  let ib = open_in_bin "price_testdata100.dat" in
  let v : Price.t array = (input_value ib) in
  v

(* test *)
let test_save_and_load = Array.for_all2 (fun x y -> x = y) prices price_loaded;;


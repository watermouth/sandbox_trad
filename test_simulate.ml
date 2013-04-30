(* test: simulate.ml *)
let trade1 = Trade.load_from_csv "trade_testdata1.csv";; 
let price1 = Price.load_from_csv "price_testdata1.csv";;
let sample_simulate1 = Simulate.simulate (Position.init Item.USD Item.JPY) price1 trade1;;
let p = Position.init Item.USD Item.JPY;;
Trade.from_array_to_string trade1;;
Price.from_array_to_string price1;;
Simulate.to_string sample_simulate1;;
print_string "press return \n";;
read_line ();;

(* from binary data *)
let trade101 = 
  let ib = open_in_bin "trade_testdata100.dat" in
  let v : Trade.t array = (input_value ib) in
  v
let price101 =
  let ib = open_in_bin "price_testdata100.dat" in
  let v : Price.t array = (input_value ib) in
  v
let p = Position.init Item.USD Item.JPY;;
let sample_simulate101 = Simulate.simulate p price101 trade101 ;;
Position.to_string 
  (Array.fold_left (fun x y -> print_string (Position.to_string x); Position.add ~mode:false x y) p trade101);;



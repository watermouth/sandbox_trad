(* direct cover test *)
let pos = Position.init Item.USD Item.JPY;;
let trades = Trade.load_from_csv "trade_testdata1.csv";; 
let cpr = Price.load_from_csv "price_testdata1.csv";;
let hpr = Price.load_from_csv "hprice_testdata1.csv";;
print_string "input delay and enter\n";;
let delay = int_of_string (read_line ());;
let nrow_mkt = Array.length cpr ;;
let nrow_trd = Array.length trades ;;
let pos_hist = Array.map
      (fun y -> let x = (Position.copy pos) in 
              x.Position.date_ <- y.Price.date_;
              x.Position.time_ <- y.Price.time_;x) cpr;; 
let info = let open Availableinfo in
  {pos_=pos_hist;cpr_=cpr;hpr_=hpr;delay_=delay};;
let cover_hash = Hashtbl.create nrow_trd;;
let (cover_trades, cover_hash) = Simulate.direct_cover 0 info (Array.to_list (Array.sub trades 0 1)) [] cover_hash;;
print_string "cpr, hpr \n";;
print_string (Price.from_array_to_string cpr);;
print_string (Price.from_array_to_string hpr);;
print_string "initial position\n";;
print_string (Position.from_array_to_string info.Availableinfo.pos_);;
print_string "input trades\n";;
print_string (Trade.from_array_to_string trades);;
print_string "cover_hash\n";;
Hashtbl.iter (fun x y -> print_string (Trade.to_string y)) cover_hash;;
print_string "cover_trd_list\n";;
Trade.from_array_to_string (Array.of_list cover_trades);;

print_string "direct cover simulation \n";; 
let sample_simulate1 = Simulate.simulate ~delay:delay (Position.init Item.USD Item.JPY) cpr hpr trades;;
print_string "position\n";;
print_string (Simulate.to_string sample_simulate1);;
 

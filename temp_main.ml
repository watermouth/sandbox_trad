(* from binary data *)
let fn_id = "101";;
let fn_in_tail = ".dat";;
let fn_trade = "tdtr";;
let trade101 = 
  let ib = open_in_bin (fn_trade ^ fn_id ^ fn_in_tail) in
  let v : Trade.t array = (input_value ib) in
  close_in ib;
  v

let fn_price = "tdpr";;
let price101 =
  let ib = open_in_bin (fn_price ^ fn_id ^ fn_in_tail) in
  let v : Price.t array = (input_value ib) in
  close_in ib;
  v

let p = Position.init Item.USD Item.JPY;;
print_string "input delay: ";;
let delay = int_of_string (read_line ());;
let (result,covers) = Simulate.simulate ~delay:delay p price101 price101 trade101;;
let fn_out_tail = ".csv";;
(* output csv files *)
Trade.to_csv ~name:(fn_trade ^ fn_id ^ fn_out_tail) trade101;;
Price.to_csv ~name:(fn_price ^ fn_id ^ fn_out_tail) price101;;
let fn_simulate = "tdsm";;
Simulate.to_csv ~name:(fn_simulate ^ fn_id ^ fn_out_tail) result;;
let fn_cover = "tdcv";;
Trade.to_csv ~name:(fn_cover ^ fn_id ^ fn_out_tail) (Array.rev (Array.of_list covers));;
let _ = ()



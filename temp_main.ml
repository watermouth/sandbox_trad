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

let _ = Simulate.simulate p price101 price101 trade101


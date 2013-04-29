(* simulator *)
(* market rate を順次処理してsimulate する *)
(* simulate *)
let simulate pos markets (trades : Trade.t array) =
  let p = ref pos in
  (* simulation steps are specified by markets *)
  let nrow_mkt = Array.length markets in
  let nrow_trd = Array.length trades in
  (* put trades into hash table *)
  (* Hashtable から取得するデータは find_allで取ると後で追加したものが先に出てくることに注意 *)
  let trd_hash =
    let h = Hashtbl.create nrow_trd in 
    (Array.iter (fun trd -> Hashtbl.add h (trd.Trade.time_) trd) trades;
    h) in
  (* I think number of cover trades is at most nrow_trd *)
  let cover_hash = Hashtbl.create nrow_trd in
  let temp_s = ref "" in 
  let (rpl, lpl, tpl) = (ref 0.0, ref 0.0, ref 0.0) in
  for i=0 to (nrow_mkt - 1) do
    (* markets.(i)を認識する時点での情報を取得する *)
    let mkt = markets.(i) in
    let time = Some mkt.Price.time_ in
    let trades = Hashtbl.find_all trd_hash time in 
    let covers = Hashtbl.find_all cover_hash time in 
    (* update *)
    p := Position.add_trade_list !p covers;
    p := Position.add_trade_list !p trades;
    (* make cover *)
    (* value *)
    let (rrpl, llpl, ttpl) = (Position.calc_pl !p mkt.Price.mid_) in
    rpl := rrpl; lpl := llpl; tpl:= ttpl;
    ignore (rpl, lpl, tpl);
    (* let s = (Position.to_string ~crlf:false !p) ^ (Printf.sprintf ", \t%10.2f,%10.2f " lpl tpl) in 
    (* Printf.printf "%s\n" s *)
    temp_s := s
    *)
    ()
  done;
  Printf.printf "%f,%f,%f" !rpl !lpl !tpl;
  print_string !temp_s;
  !p
    
(* cover *) 
(* Hashtblを使えば良さそう *)

(* test *)
;;
let trade1 = Trade.load_from_csv "trade_testdata1.csv";; 
let price1 = Price.load_from_csv "price_testdata1.csv";;
let sample_simulate1 = simulate (Position.init Item.USD Item.JPY) price1 trade1;;
let p = Position.init Item.USD Item.JPY;;
Trade.from_array_to_string trade1;;
Price.from_array_to_string price1;;
Position.to_string 
  (Array.fold_left (fun x y -> print_string (Position.to_string x); Position.add ~mode:false x y) p trade1);;
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
Position.to_string 
  (Array.fold_left (fun x y -> print_string (Position.to_string x); Position.add ~mode:false x y) p trade101);;



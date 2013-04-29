(* simulator *)
(* market rate ��缡��������simulate ���� *)
(* simulate *)
let simulate pos markets trades =
  let p = ref pos in
  (* simulation steps are specified by markets *)
  let nrow_mkt = Array.length markets in
  let nrow_trd = Array.length trades in
  (* put trades into hash table *)
  (* Hashtable �����������ǡ����� find_all�Ǽ��ȸ���ɲä�����Τ���˽ФƤ��뤳�Ȥ���� *)
  let trd_hash =
    let h = Hashtbl.create nrow_trd in 
    (Array.iter (fun trd -> Hashtbl.add h (trd.Trade.time_) trd) trades;
    h) in
  (* I think number of cover trades is at most nrow_trd *)
  let cover_hash = Hashtbl.create nrow_trd in
  for i=0 to (nrow_mkt - 1) do
    (* markets.(i)��ǧ����������Ǥξ����������� *)
    let mkt = markets.(i) in
    let time = Some mkt.Price.time_ in
    let trades = Hashtbl.find_all trd_hash time in 
    let covers = Hashtbl.find_all cover_hash time in 
    (* update *)
    p := Position.add_trade_list !p covers;
    p := Position.add_trade_list !p trades;
    (* make cover *)
    (* value *)
    ()
  done;
  !p
    
(* cover *) 
(* Hashtbl��Ȥ����ɤ����� *)

(* test *)
;;
let trade1 = Trade.load_from_csv "trade_testdata1.csv";; 
let price1 = Price.load_from_csv "price_testdata1.csv";;
let sample_simulate1 = simulate (Position.init Item.USD Item.JPY) price1 trade1;;
let p = Position.init Item.USD Item.JPY;;
Array.fold_left (fun x y -> Position.to_string x; Position.add ~mode:false x y) p trade1;;


(* simulator *)

type t = {
  nrow_:	int;
  pos_hist_:	Position.t array; (* position history *)
  pl_latent_:	float array;
  pl_total_:	float array;
  bid_:		float array;
  ask_:		float array;
  h_bid_:	float array;
  h_ask_:	float array;
}
  
(* market rate を順次処理してsimulate する *)
(* simulate *)
let simulate pos markets (trades : Trade.t array) =
  let p = ref pos in
  (* simulation steps are specified by markets *)
  let nrow_mkt = Array.length markets in
  let nrow_trd = Array.length trades in
  let pos_hist = Array.create nrow_mkt pos in 
  let lpl = Array.create nrow_mkt 0.0 in 
  let tpl = Array.create nrow_mkt 0.0 in 
  let bid = Array.create nrow_mkt 0.0 in 
  let ask = Array.create nrow_mkt 0.0 in 
  let h_bid = Array.create nrow_mkt 0.0 in 
  let h_ask = Array.create nrow_mkt 0.0 in 
  (* put trades into hash table *)
  (* Hashtable から取得するデータは find_allで取ると後で追加したものが先に出てくることに注意 *)
  let trd_hash =
    let h = Hashtbl.create nrow_trd in 
    (Array.iter (fun trd -> Hashtbl.add h (trd.Trade.time_) trd) trades;
    h) in
  (* I think number of cover trades is at most nrow_trd *)
  let cover_hash = Hashtbl.create nrow_trd in
  (* latent pl, total pl *)
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
    pos_hist.(i) <- !p;
    let (_, llpl, ttpl) = (Position.calc_pl !p mkt.Price.mid_) in
    lpl.(i) <- llpl; tpl.(i) <- ttpl; 
    bid.(i) <- mkt.Price.bid_;
    ask.(i) <- mkt.Price.ask_;
    ()
  done;
  {nrow_=nrow_mkt;pos_hist_=pos_hist; pl_latent_=lpl; pl_total_=tpl;
   bid_=bid; ask_=ask; h_bid_=h_bid; h_ask_=h_ask}
 
let to_string 
  {nrow_=nrow; pos_hist_=pos_hist; pl_latent_=lpl; pl_total_=tpl;
   bid_=bid; ask_=ask; h_bid_=h_bid; h_ask_=h_ask} =
   let s = ref "" in
   for i=0 to (nrow-1) do
     s := !s ^ (Printf.sprintf "%s,%10.2f,%10.2f,%7.5f,%7.5f,%7.5f,%7.5f\n"
                 (Position.to_string ~crlf:false pos_hist.(i))
                 lpl.(i) tpl.(i) bid.(i) ask.(i) h_bid.(i) h_ask.(i))
   done;
   !s

    

(* cover *) 
(* Hashtblを使えば良さそう *)



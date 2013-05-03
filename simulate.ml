(* simulator *)
open CalendarLib
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

(* cover rule: direct *)
let direct_cover (idx:int) info pos trd_list cvtrd_list cover_hash = 
  let open Trade in
  let rec sub trades result = match trades with
  | [] -> result
  | h::t -> 
    let execution_time = Datetimehelper.add_second (pos.Position.time_) info.Availableinfo.delay_ in
    (* cover trade *)
    let cv = (Trade.make (pos.Position.seq_) (pos.Position.date_) (Some execution_time) "cover"
                (Item.to_int (pos.Position.item1_)) (~-. (h.lot1_)) (Item.to_int (pos.Position.item2_)) None) in
    Hashtbl.add cover_hash execution_time cv;
    sub t (cv :: result)
  in (sub trd_list [], cover_hash)
  
(* market rate を順次処理してsimulate する *)
(* simulate *)
let simulate ?(delay=0) ?(cover_rule=direct_cover) pos cpr hpr (trades : Trade.t array) =
  (* simulation steps are specified by cpr *)
  let nrow_mkt = Array.length cpr in
  let nrow_trd = Array.length trades in
  let pos_hist = Array.make nrow_mkt (Position.copy pos) in 
  let lpl = Array.create nrow_mkt 0.0 in 
  let tpl = Array.create nrow_mkt 0.0 in 
  let bid = Array.create nrow_mkt 0.0 in 
  let ask = Array.create nrow_mkt 0.0 in 
  let h_bid = Array.create nrow_mkt 0.0 in 
  let h_ask = Array.create nrow_mkt 0.0 in 
  (* info *)
  let info = let open Availableinfo in
    {pos_=pos_hist;cpr_=cpr;hpr_=hpr;delay_=delay} in 
  (* put trades into hash table *)
  (* Hashtable から取得するデータは find_allで取ると後で追加したものが先に出てくることに注意 *)
  let trd_hash =
    let h = Hashtbl.create nrow_trd in 
    (Array.iter (fun trd -> Hashtbl.add h 
      (match trd.Trade.time_ with Some time -> time | None -> assert false) trd)
      trades;
    h) in
  (* I think number of cover trades is at most nrow_trd *)
  let cover_hash = Hashtbl.create nrow_trd in 
  let cover_result = ref [] in 
  (* position state variable *)
  let p = ref pos in
  !p.Position.date_ <- cpr.(0).Price.date_;
  for i=0 to (nrow_mkt - 1) do
    (* cpr.(i)を認識する時点での情報を取得する *)
    let mkt = cpr.(i) in
    let time = mkt.Price.time_ in
    let trades = Hashtbl.find_all trd_hash time in 
    let covers = Hashtbl.find_all cover_hash time in 
    (* set price to cover trades *)
    let covers = List.map (fun trd -> Tradehandler.set_baprice trd mkt) covers in
    (* update *)
    !p.Position.time_ <- time; 
    p := Position.add_trade_list !p covers;
    p := Position.add_trade_list !p trades;
    (* make cover *)
    let (covers_made, cover_hash) = cover_rule i (info) !p trades covers cover_hash in 
    cover_result := (List.append covers_made !cover_result);
    (* delay = 0 is exceptional case *)
    p := if delay != 0 then !p 
         else 
           (let covers_made = List.map (fun trd -> Tradehandler.set_baprice trd mkt) covers_made in
           (* print_string (Trade.from_array_to_string (Array.of_list covers_made));*)
           (Position.add_trade_list !p covers_made));
    (* latent pl, total pl *)
    let (_, llpl, ttpl) = (Position.calc_pl !p mkt.Price.mid_) in
    (* record *)
    pos_hist.(i) <- Position.copy (!p); (* COPY !! *)
    lpl.(i) <- llpl; tpl.(i) <- ttpl; 
    bid.(i) <- mkt.Price.bid_;
    ask.(i) <- mkt.Price.ask_;
    h_bid.(i) <- hpr.(i).Price.bid_;
    h_ask.(i) <- hpr.(i).Price.ask_;
    ()
  done;
  ({nrow_=nrow_mkt;pos_hist_=pos_hist; pl_latent_=lpl; pl_total_=tpl;
   bid_=bid; ask_=ask; h_bid_=h_bid; h_ask_=h_ask},
   (!cover_result))
 
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

let to_csv ~name 
  {nrow_=nrow; pos_hist_=pos_hist; pl_latent_=lpl; pl_total_=tpl;
   bid_=bid; ask_=ask; h_bid_=h_bid; h_ask_=h_ask} =
   let oc = open_out name in
   for i=0 to (nrow-1) do
     output_string oc
       (Printf.sprintf "%s,%10.2f,%10.2f,%7.5f,%7.5f,%7.5f,%7.5f\n"
           (Position.to_string ~crlf:false pos_hist.(i))
           lpl.(i) tpl.(i) bid.(i) ask.(i) h_bid.(i) h_ask.(i))
   done;
   close_out oc 
    

(* cover *) 



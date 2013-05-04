(* from binary data *)
(* input from argumetns *)
let do_output_csv = bool_of_string (Sys.argv.(1));; 
let fn_id = Sys.argv.(2);; (* data id *) 
let delay = int_of_string (Sys.argv.(3));;
let rule_number = int_of_string (Sys.argv.(4));;
Printf.printf "delay:%d,rule:%d," delay rule_number
let cover_rule = match (rule_number) with
  | 1 -> Simulate.direct_cover
  | 2 -> let lot_limit = (float_of_string Sys.argv.(5)) in
         let lot_left  = (float_of_string Sys.argv.(6)) in
    Printf.printf "limit:%7.0f,left:%7.0f," lot_limit lot_left;
    Coverlotlimit.get ~lot_limit:lot_limit ~lot_left:lot_left
  | _ -> raise Not_found
;;
Printf.printf "pl:"
(* Printf.printf "input cover rule: %d\n direct:1, lotlimit:2\n" rule_number ;;*)

(* load data *)
let fn_in_tail = ".dat";;
let fn_trade = "tdtr";;
let trades = 
  let ib = open_in_bin (fn_trade ^ fn_id ^ fn_in_tail) in
  let v : Trade.t array = (input_value ib) in
  close_in ib;
  v

let fn_price = "tdpr";;
let cpr =
  let ib = open_in_bin (fn_price ^ fn_id ^ fn_in_tail) in
  let v : Price.t array = (input_value ib) in
  close_in ib;
  v

(* simulation *)
let p = Position.init Item.USD Item.JPY;;
let (result,covers) = Simulate.simulate ~cover_rule:cover_rule ~delay:delay p cpr cpr trades;;
let pl =
  let a = (result.Simulate.pl_total_) in
  let len = Array.length a in
  a.(len-1);;
Printf.printf "%0.2f\n" pl;;

(* output csv files *)
let out_csvs () = 
  let fn_out_tail = ".csv" in
  let fn_simulate = "tdsm" in
  let fn_cover = "tdcv" in
  Trade.to_csv ~name:(fn_trade ^ fn_id ^ fn_out_tail) trades;
  Price.to_csv ~name:(fn_price ^ fn_id ^ fn_out_tail) cpr;
  Simulate.to_csv ~name:(fn_simulate ^ fn_id ^ fn_out_tail) result;
  Trade.to_csv ~name:(fn_cover ^ fn_id ^ fn_out_tail) (Batteries.Array.rev (Array.of_list covers));;

let _ = if do_output_csv then out_csvs () else ()



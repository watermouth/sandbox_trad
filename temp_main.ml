(* from binary data *)
(* input from argumetns *)
let do_output_csv = bool_of_string (Sys.argv.(1));; 
let fn_id = Sys.argv.(2);; (* data id *) 
let delay = int_of_string (Sys.argv.(3));;
let rule_number = int_of_string (Sys.argv.(4));;
let lot_limit =  ref 0.0;;
let lot_left  =  ref 0.0;;
let pl_upper  =  ref 0.0;;
let pl_lower  =  ref 0.0;;
let cover_rule = match (rule_number) with
  | 1 -> Simulate.direct_cover
  | 2 -> lot_limit := (float_of_string Sys.argv.(5));
         lot_left  := (float_of_string Sys.argv.(6));
         Coverlotlimit.get ~lot_limit:!lot_limit ~lot_left:!lot_left
  | 3 -> lot_limit := (float_of_string Sys.argv.(5));
         lot_left  := (float_of_string Sys.argv.(6));
         pl_upper  := (float_of_string Sys.argv.(7));
         pl_lower  := (float_of_string Sys.argv.(8));
         Coverpl.get ~lot_limit:!lot_limit ~lot_left:!lot_left ~pl_upper:!pl_upper ~pl_lower:!pl_lower  
  | _ -> raise Not_found
;;
(* Printf.printf "input cover rule: %d\n direct:1, lotlimit:2\n" rule_number ;; *)

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
(* output daily result *)
let header = 
  "data_id,delay,rule,pl,lot_limit,lot_left,pl_upper,pl_lower\n";; 
let daily_result_string = match (rule_number) with
  | 1 -> Printf.sprintf "%s,%d,%d,%0.2f,,,," fn_id delay rule_number pl 
  | 2 -> Printf.sprintf "%s,%d,%d,%0.2f,%0.0f,%0.0f,," fn_id delay rule_number pl !lot_limit !lot_left  
  | 3 -> Printf.sprintf "%s,%d,%d,%0.2f,%0.0f,%0.0f,%5.4f,%5.4f" 
                       fn_id delay rule_number pl !lot_limit !lot_left !pl_upper !pl_lower
  | _ -> raise Not_found;;
Printf.printf "%s\n" daily_result_string ;;

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



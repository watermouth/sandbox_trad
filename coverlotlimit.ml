(* cover rule: lot limit *)
module P = Position;;
let cover_ ?(lot_limit=100000.0) ?(lot_left=10000.0) (idx:int) info pos trd_list cvtrd_list cover_hash = 
  let open Trade in
  let diff lot = if lot >= lot_limit && lot > lot_left then lot -. lot_left else 0.0 in
  let lot = pos.P.lot1_ in
  let cover_lot = if lot >= 0.0 then ~-. (diff lot)
                  else (diff (~-.lot)) in
  (* cover trade *)
  if abs_float cover_lot = 0.0 then ([], cover_hash)
  else 
    let execution_time = Datetimehelper.add_second (pos.P.time_) info.Availableinfo.delay_ in
    let cv = (Trade.make (pos.P.seq_) pos.P.date_ (Some execution_time) "cover"
                (Item.to_int (pos.P.item1_)) (cover_lot) (Item.to_int (pos.P.item2_)) None) in
    Hashtbl.add cover_hash execution_time cv;
    ([cv], cover_hash)

let get ~lot_limit ~lot_left = cover_ ~lot_limit ~lot_left;;



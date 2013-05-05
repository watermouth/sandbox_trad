(* cover rule: pl *)
module P = Position;;

let cover_ ?(lot_limit=100000.0) ?(lot_left=10000.0)
  ?(pl_upper=0.1) ?(pl_lower= ~-. 0.1)
  (idx:int) info pos trd_list cvtrd_list cover_hash = 
   match pos.P.lot1_ with
  | 0.0 -> ([], cover_hash)
  | lot -> 
    let diff_lot x = if x > lot_left then x -. lot_left else 0.0 in 
    let cpr = info.Availableinfo.cpr_.(idx) in
    let pl = (Positionhandler.calc_unit_pl pos cpr) in
    (* Printf.printf "open position unit pl:%f\n" pl; *)
    (* pl bound check *)
    let cover_lot =  
      (* do cover *)
      if pl >= pl_upper || pl <= pl_lower then
        if lot > 0.0 then ~-. (diff_lot lot) else diff_lot (~-.lot)
      else 0.0 in 
    (* Printf.printf "cover_lot:%f\n" cover_lot; *)
    if (cover_lot <> 0.0) then 
      let execution_time = Datetimehelper.add_second (pos.P.time_) info.Availableinfo.delay_ in
      let cv = (Trade.make (pos.P.seq_) pos.P.date_ (Some execution_time) "cover"
                  (Item.to_int (pos.P.item1_)) (cover_lot) (Item.to_int (pos.P.item2_)) None) in
      Hashtbl.add cover_hash execution_time cv;
      ([cv], cover_hash)
    else (* pl cover *)
      ((* Printf.printf "lot_limit:%f, lot_left:%f\n" lot_limit lot_left; *)
      Coverlotlimit.cover_ ~lot_limit ~lot_left idx info pos trd_list cvtrd_list cover_hash) 

let get ~lot_limit ~lot_left ~pl_upper ~pl_lower
   = cover_ ~lot_limit ~lot_left ~pl_upper ~pl_lower;;



(* script generation *)
(* for rule 2: lot limit and lot left *)
let script_gen_2 ?(lotlimitunit=10000) ?(lotleftunit=10000) fn_id = 
  let fn = ("batch" ^ fn_id ^ "_2_" ^ ".sh") in 
  let oc = open_out fn in
  output_string oc "#!/bin/sh\n";
  for k=0 to 2 do
  for i=0 to 10 do
  for j=0 to 10 do
    output_string oc (Printf.sprintf "./a.out false %s %d 2 %d %d\n" fn_id k (lotlimitunit*i) (lotleftunit*j))
  done; done; done;
  close_out oc;
  Unix.system ("chmod u+x " ^ fn) 
  ;;

(* for rule 3: lot limit, lot left, pl upper, pl lower *)
let script_gen_3
  ?(lotlimitunit=1000000) ?(lotleftunit=0) ?(plu=0.01) ?(pll= ~-. 0.01) fn_id =  
  let fn = ("batch" ^ fn_id ^ "_3_" ^ ".sh") in
  let oc = open_out fn in
  output_string oc "#!/bin/sh\n";
  for k=0 to 2 do
  for i=0 to 10 do
  for j=0 to 10 do
    output_string oc (Printf.sprintf "./a.out false %s %d 3 %d %d %f %f\n"
      fn_id k lotlimitunit lotleftunit 
      ((plu) *. (float_of_int i)) ((pll) *. (float_of_int j)))
  done; done; done;
  close_out oc;
  Unix.system ("chmod u+x " ^ fn)
  ;;

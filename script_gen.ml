(* script generation *)
print_string "input id:";;
let fn_id = read_line ();;
let oc = open_out "batch.sh";;
output_string oc "#!/bin/sh\n";
for k=0 to 2 do
for i=0 to 10 do
for j=0 to 10 do
      output_string oc (Printf.sprintf "./a.out false %s %d 2 %d %d\n" fn_id k (10000*i) (10000*j))
done; done; done;
close_out oc;;

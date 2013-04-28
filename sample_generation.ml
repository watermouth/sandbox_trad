(* sample data generation *)
(* sample trades *)
print_string "sample output file name:";;
let trd_ofn = read_line ();;
output_file (trd_ofn) (Trade.from_array_to_string (Trade.make_samples 10));;

(* sample prices *)
Printf.printf "sample output file name(price)\n:";;
let cp_ofn = read_line ();;
output_file (cp_ofn) (Price.from_array_to_string (Price.make_samples 10));;


